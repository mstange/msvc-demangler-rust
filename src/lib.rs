//===- MicrosoftDemangle.cpp ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a demangler for MSVC-style mangled symbols.
//
// This file has no dependencies on the rest of LLVM so that it can be
// easily reused in other programs such as libcxxabi.
//
//===----------------------------------------------------------------------===//

#[macro_use]
extern crate bitflags;

use std::io::Write;
use std::result;
use std::str;
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    s: String,
}

impl Error {
    fn new(s: String) -> Error {
        Error { s }
    }
}

impl From<std::str::Utf8Error> for Error {
    fn from(t: std::str::Utf8Error) -> Error {
        Error {
            s: format!("{:?}", t),
        }
    }
}
impl From<std::string::FromUtf8Error> for Error {
    fn from(t: std::string::FromUtf8Error) -> Error {
        Error {
            s: format!("{:?}", t),
        }
    }
}

#[derive(Debug, Clone)]
struct SerializeError {
    s: String,
}

impl From<std::str::Utf8Error> for SerializeError {
    fn from(err: std::str::Utf8Error) -> SerializeError {
        SerializeError {
            s: format!("{:?}", err),
        }
    }
}

impl From<std::io::Error> for SerializeError {
    fn from(err: std::io::Error) -> SerializeError {
        SerializeError {
            s: format!("{:?}", err),
        }
    }
}

type SerializeResult<T> = result::Result<T, SerializeError>;

pub type Result<T> = result::Result<T, Error>;

bitflags! {
    pub struct StorageClass: u32 {
        const CONST      = 0b00000001;
        const VOLATILE   = 0b00000010;
        const FAR        = 0b00000100;
        const HUGE       = 0b00001000;
        const UNALIGNED  = 0b00010000;
        const RESTRICT   = 0b00100000;
    }
}

#[derive(PartialEq)]
pub enum DemangleFlags {
    LessWhitespace,
    LotsOfWhitespace,
}

// Calling conventions
enum CallingConv {
    Cdecl,
    Pascal,
    Thiscall,
    Stdcall,
    Fastcall,
    _Regcall,
}

bitflags! {
    struct FuncClass: u32 {
        const PUBLIC     = 0b00000001;
        const PROTECTED  = 0b00000010;
        const PRIVATE    = 0b00000100;
        const GLOBAL     = 0b00001000;
        const STATIC     = 0b00010000;
        const VIRTUAL    = 0b00100000;
        const FAR        = 0b01000000;
    }
}

// Represents an identifier which may be a template.
#[derive(Clone, Debug, PartialEq)]
pub enum Name<'a> {
    Operator(&'static str),
    NonTemplate(&'a [u8]),
    Template(Box<Name<'a>>, Params<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct NameSequence<'a> {
    pub names: Vec<Name<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Params<'a> {
    pub types: Vec<Type<'a>>,
}

// The type class. Mangled symbols are first parsed and converted to
// this type and then converted to string.
#[derive(Clone, Debug, PartialEq)]
pub enum Type<'a> {
    None,
    MemberFunction(Params<'a>, StorageClass, Box<Type<'a>>),
    NonMemberFunction(Params<'a>, StorageClass, Box<Type<'a>>),
    CXXVBTable(NameSequence<'a>, StorageClass),
    CXXVFTable(NameSequence<'a>, StorageClass),
    TemplateParameterWithIndex(i32),
    Constant(i32),
    Ptr(Box<Type<'a>>, StorageClass),
    Ref(Box<Type<'a>>, StorageClass),
    Array(i32, Box<Type<'a>>, StorageClass),

    Struct(NameSequence<'a>, StorageClass),
    Union(NameSequence<'a>, StorageClass),
    Class(NameSequence<'a>, StorageClass),
    Enum(NameSequence<'a>, StorageClass),

    Void(StorageClass),
    Bool(StorageClass),
    Char(StorageClass),
    Schar(StorageClass),
    Uchar(StorageClass),
    Short(StorageClass),
    Ushort(StorageClass),
    Int(StorageClass),
    Uint(StorageClass),
    Long(StorageClass),
    Ulong(StorageClass),
    Int64(StorageClass),
    Uint64(StorageClass),
    Wchar(StorageClass),
    Float(StorageClass),
    Double(StorageClass),
    Ldouble(StorageClass),
    VarArgs,
}

#[derive(Debug)]
pub struct ParseResult<'a> {
    pub symbol: NameSequence<'a>,
    pub symbol_type: Type<'a>,
}

// Demangler class takes the main role in demangling symbols.
// It has a set of functions to parse mangled symbols into Type instnaces.
// It also has a set of functions to cnovert Type instances to strings.
struct ParserState<'a> {
    // Mangled symbol. read_* functions shorten this string
    // as they parse it.
    input: &'a [u8],

    // The first 10 names in a mangled name can be back-referenced by
    // special name @[0-9]. This is a storage for the first 10 names.
    memorized_names: Vec<Name<'a>>,

    memorized_types: Vec<Type<'a>>,
}

impl<'a> ParserState<'a> {
    fn parse(mut self) -> Result<ParseResult<'a>> {
        // MSVC-style mangled symbols must start with b'?'.
        if !self.consume(b"?") {
            return Err(Error::new("does not start with b'?'".to_owned()));
        }

        if self.consume(b"$") {
            let name = self.read_template_name()?;
            return Ok(ParseResult {
                symbol: NameSequence { names: vec![name] },
                symbol_type: Type::None,
            });
        }

        // What follows is a main symbol name. This may include
        // namespaces or class names.
        let symbol = self.read_name(true)?;

        if let Ok(c) = self.get() {
            let symbol_type = match c {
                b'0'...b'5' => {
                    // Read a variable.
                    self.read_var_type(StorageClass::empty())?
                }
                b'6' => {
                    let access_class = self.read_func_access_class();
                    let name = self.read_name(false)?;
                    Type::CXXVFTable(name, access_class)
                }
                b'7' => {
                    let access_class = self.read_func_access_class();
                    let name = self.read_name(false)?;
                    Type::CXXVBTable(name, access_class)
                }
                b'Y' => {
                    // Read a non-member function.
                    let _ = self.read_calling_conv()?;
                    let storage_class = self.read_storage_class_for_return()?;
                    let return_type = self.read_var_type(storage_class)?;
                    let params = self.read_params()?;
                    Type::NonMemberFunction(params, StorageClass::empty(), Box::new(return_type))
                }
                c => {
                    // Read a member function.
                    let _func_lass = self.read_func_class(c)?;
                    let _is_64bit_ptr = self.expect(b"E");
                    let access_class = self.read_func_access_class();
                    let _calling_conv = self.read_calling_conv()?;
                    let storage_class_for_return = self.read_storage_class_for_return()?;
                    let return_type = self.read_func_return_type(storage_class_for_return)?;
                    let params = self.read_params()?;
                    Type::MemberFunction(params, access_class, Box::new(return_type))
                }
            };
            Ok(ParseResult {
                symbol,
                symbol_type,
            })
        } else {
            Ok(ParseResult {
                symbol,
                symbol_type: Type::None,
            })
        }
    }

    fn peek(&self) -> Option<u8> {
        self.input.first().cloned()
    }

    fn get(&mut self) -> Result<u8> {
        match self.peek() {
            Some(first) => {
                self.trim(1);
                Ok(first)
            }
            None => {panic!("Unexpected end of input");}// Err(Error::new("unexpected end of input".to_owned())),
        }
    }

    fn consume(&mut self, s: &[u8]) -> bool {
        if self.input.starts_with(s) {
            self.trim(s.len());
            true
        } else {
            false
        }
    }

    fn trim(&mut self, len: usize) {
        self.input = &self.input[len..]
    }

    fn expect(&mut self, s: &[u8]) -> Result<()> {
        if !self.consume(s) {
            return Err(Error::new(format!(
                "{} expected, but got {}",
                str::from_utf8(s)?,
                str::from_utf8(self.input)?
            )));
        }
        Ok(())
    }

    fn consume_digit(&mut self) -> Option<u8> {
        match self.peek() {
            Some(first) => {
                if char::from(first).is_digit(10) {
                    self.trim(1);
                    Some(first - b'0')
                } else {
                    None
                }
            }
            None => None,
        }
    }

    // Sometimes numbers are encoded in mangled symbols. For example,
    // "int (*x)[20]" is a valid C type (x is a pointer to an array of
    // length 20), so we need some way to embed numbers as part of symbols.
    // This function parses it.
    //
    // <number>               ::= [?] <non-negative integer>
    //
    // <non-negative integer> ::= <decimal digit> # when 1 <= Number <= 10
    //                        ::= <hex digit>+ @  # when Numbrer == 0 or >= 10
    //
    // <hex-digit>            ::= [A-P]           # A = 0, B = 1, ...
    fn read_number(&mut self) -> Result<i32> {
        let neg = self.consume(b"?");

        if let Some(digit) = self.consume_digit() {
            let ret = digit + 1;
            return Ok(if neg { -(ret as i32) } else { ret as i32 });
        }

        let orig = self.input;
        let mut i = 0;
        let mut ret = 0;
        for c in self.input {
            match *c {
                b'@' => {
                    self.trim(i + 1);
                    return Ok(if neg { -(ret as i32) } else { ret as i32 });
                }
                b'A'...b'P' => {
                    ret = (ret << 4) + ((c - b'A') as i32);
                    i += 1;
                }
                _ => {
                    return Err(Error::new(format!("bad number: {}", str::from_utf8(orig)?)));
                }
            }
        }
        Err(Error::new(format!("bad number: {}", str::from_utf8(orig)?)))
    }

    // Read until the next b'@'.
    fn read_string(&mut self) -> Result<&'a [u8]> {
        if let Some(pos) = self.input.iter().position(|&x| x == b'@') {
            let ret = &self.input[0..pos];
            self.trim(pos + 1);
            Ok(ret)
        } else {
            let error = format!("read_string: missing b'@': {}", str::from_utf8(self.input)?);
            Err(Error::new(error))
        }
    }

    // First 10 strings can be referenced by special names ?0, ?1, ..., ?9.
    // Memorize it.
    fn memorize_name(&mut self, n: &Name<'a>) {
        // TODO: the contains check does an equality check on the Name enum, which
        // might do unexpected things in subtle cases. It's not a pure string equality check.
        // println!("memorize name {:?}", n);
        if self.memorized_names.len() < 10 && !self.memorized_names.contains(n) {
            self.memorized_names.push(n.clone());
        }
    }
    fn memorize_type(&mut self, t: &Type<'a>) {
        // TODO: the contains check does an equality check on the Type enum, which
        // might do unexpected things in subtle cases. It's not a pure string equality check.
        if self.memorized_types.len() < 10 && !self.memorized_types.contains(t) {
            self.memorized_types.push(t.clone());
        }
    }

    fn read_template_name(&mut self) -> Result<Name<'a>> {
        // Templates have their own context for backreferences.
        let saved_memorized_names = mem::replace(&mut self.memorized_names, vec![]);
        let saved_memorized_types = mem::replace(&mut self.memorized_types, vec![]);
        let name = self.read_unqualified_name(false)?; // how does wine deal with ??$?DM@std@@YA?AV?$complex@M@0@ABMABV10@@Z
        let template_params = self.read_params()?;
        let _ = mem::replace(&mut self.memorized_names, saved_memorized_names);
        let _ = mem::replace(&mut self.memorized_types, saved_memorized_types);
        if !self.input.is_empty() {
            self.expect(b"@")?; // TODO: Can this be ignored?
        }
        Ok(Name::Template(Box::new(name), template_params))
    }

    fn read_unqualified_name(&mut self, function: bool) -> Result<Name<'a>> {
        let orig = self.input;
        let name = if let Some(i) = self.consume_digit() {
            let i = i as usize;
            if i >= self.memorized_names.len() {
                return Err(Error::new(format!(
                    "name reference too large: {}",
                    str::from_utf8(orig)?
                )));
            }
            // println!("reading memorized name in position {}", i);
            // println!(
            //    "current list of memorized_names: {:#?}",
            //    self.memorized_names
            // );
            self.memorized_names[i].clone()
        } else if self.consume(b"?$") {
            let name = self.read_template_name()?;
            if !function {
                self.memorize_name(&name);
            }
            name
        } else if self.consume(b"?") {
            // Overloaded operator.
            self.read_operator()?
        } else {
            // Non-template functions or classes.
            let name = self.read_string()?;
            let name = Name::NonTemplate(name);
            self.memorize_name(&name);
            name
        };
        Ok(name)
    }

    // Parses a name in the form of A@B@C@@ which represents C::B::A.
    fn read_name(&mut self, mut function: bool) -> Result<NameSequence<'a>> {
        // println!("read_name on {}", str::from_utf8(self.input)?);
        let mut names = Vec::new();
        while !self.consume(b"@") {
            // println!("read_name iteration on {}", str::from_utf8(self.input)?);
            let name = self.read_unqualified_name(function)?;
            function = false;
            names.push(name);
        }

        Ok(NameSequence { names })
    }

    fn read_func_ptr(&mut self, sc: StorageClass) -> Result<Type<'a>> {
        let return_type = self.read_var_type(StorageClass::empty())?;
        let params = self.read_params()?;

        if self.input.starts_with(b"@Z") {
            self.trim(2);
        } else if self.input.starts_with(b"Z") {
            self.trim(1);
        }

        Ok(Type::Ptr(
            Box::new(Type::NonMemberFunction(
                params,
                StorageClass::empty(),
                Box::new(return_type),
            )),
            sc,
        ))
    }

    fn read_operator(&mut self) -> Result<Name<'a>> {
        Ok(Name::Operator(self.read_operator_name()?))
    }

    fn read_operator_name(&mut self) -> Result<&'static str> {
        let orig = self.input;

        Ok(match self.get()? {
            b'0' => "ctor",
            b'1' => "dtor",
            b'2' => " new",
            b'3' => " delete",
            b'4' => "=",
            b'5' => ">>",
            b'6' => "<<",
            b'7' => "!",
            b'8' => "==",
            b'9' => "!=",
            b'A' => "[]",
            b'B' => "cast", // TODO
            b'C' => "->",
            b'D' => "*",
            b'E' => "++",
            b'F' => "--",
            b'G' => "-",
            b'H' => "+",
            b'I' => "&",
            b'J' => "->*",
            b'K' => "/",
            b'L' => "%",
            b'M' => "<",
            b'N' => "<=",
            b'O' => ">",
            b'P' => ">=",
            b'Q' => ",",
            b'R' => "()",
            b'S' => "~",
            b'T' => "^",
            b'U' => "|",
            b'V' => "&&",
            b'W' => "||",
            b'X' => "*=",
            b'Y' => "+=",
            b'Z' => "-=",
            b'_' => match self.get()? {
                b'0' => "/=",
                b'1' => "%=",
                b'2' => ">>=",
                b'3' => "<<=",
                b'4' => "&=",
                b'5' => "|=",
                b'6' => "^=",
                b'7' => "vftable",
                b'8' => "vbtable",
                b'9' => "vcall",
                b'F' => "ctor_DefaultClosure", // TODO
                b'O' => "ctor_CopyingClosure", // TODO
                b'U' => " new[]",
                b'V' => " delete[]",
                b'_' => if self.consume(b"L") {
                    " co_await"
                } else if self.consume(b"K") {
                    " CXXLiteralOperatorName" // TODO: read <source-name>, that's the operator name
                } else {
                    return Err(Error::new(format!(
                        "unknown operator name: {}",
                        str::from_utf8(orig)?
                    )));
                },
                _ => {
                    return Err(Error::new(format!(
                        "unknown operator name: {}",
                        str::from_utf8(orig)?
                    )))
                }
            },
            _ => {
                return Err(Error::new(format!(
                    "unknown operator name: {}",
                    str::from_utf8(orig)?
                )))
            }
        })
    }

    fn read_func_class(&mut self, c: u8) -> Result<FuncClass> {
        Ok(match c {
            b'A' => FuncClass::PRIVATE,
            b'B' => FuncClass::PRIVATE | FuncClass::FAR,
            b'C' => FuncClass::PRIVATE | FuncClass::STATIC,
            b'D' => FuncClass::PRIVATE | FuncClass::STATIC,
            b'E' => FuncClass::PRIVATE | FuncClass::VIRTUAL,
            b'F' => FuncClass::PRIVATE | FuncClass::VIRTUAL,
            b'I' => FuncClass::PROTECTED,
            b'J' => FuncClass::PROTECTED | FuncClass::FAR,
            b'K' => FuncClass::PROTECTED | FuncClass::STATIC,
            b'L' => FuncClass::PROTECTED | FuncClass::STATIC | FuncClass::FAR,
            b'M' => FuncClass::PROTECTED | FuncClass::VIRTUAL,
            b'N' => FuncClass::PROTECTED | FuncClass::VIRTUAL | FuncClass::FAR,
            b'Q' => FuncClass::PUBLIC,
            b'R' => FuncClass::PUBLIC | FuncClass::FAR,
            b'S' => FuncClass::PUBLIC | FuncClass::STATIC,
            b'T' => FuncClass::PUBLIC | FuncClass::STATIC | FuncClass::FAR,
            b'U' => FuncClass::PUBLIC | FuncClass::VIRTUAL,
            b'V' => FuncClass::PUBLIC | FuncClass::VIRTUAL | FuncClass::FAR,
            b'Y' => FuncClass::GLOBAL,
            b'Z' => FuncClass::GLOBAL | FuncClass::FAR,
            _ => {
                return Err(Error::new(format!(
                    "unknown func class: {}",
                    str::from_utf8(&[c])?
                )))
            }
        })
    }

    fn read_func_access_class(&mut self) -> StorageClass {
        let access_class = match self.peek() {
            Some(b'A') => StorageClass::empty(),
            Some(b'B') => StorageClass::CONST,
            Some(b'C') => StorageClass::VOLATILE,
            Some(b'D') => StorageClass::CONST | StorageClass::VOLATILE,
            _ => return StorageClass::empty(),
        };
        self.trim(1);
        access_class
    }

    fn read_calling_conv(&mut self) -> Result<CallingConv> {
        let orig = self.input;

        Ok(match self.get()? {
            b'A' => CallingConv::Cdecl,
            b'B' => CallingConv::Cdecl,
            b'C' => CallingConv::Pascal,
            b'E' => CallingConv::Thiscall,
            b'G' => CallingConv::Stdcall,
            b'I' => CallingConv::Fastcall,
            _ => {
                return Err(Error::new(format!(
                    "unknown calling conv: {}",
                    str::from_utf8(orig)?
                )))
            }
        })
    }

    // <return-type> ::= <type>
    //               ::= @ # structors (they have no declared return type)
    fn read_func_return_type(&mut self, storage_class: StorageClass) -> Result<Type<'a>> {
        if self.consume(b"@") {
            Ok(Type::None)
        } else {
            self.read_var_type(storage_class)
        }
    }

    fn read_storage_class(&mut self) -> StorageClass {
        let storage_class = match self.peek() {
            Some(b'A') => StorageClass::empty(),
            Some(b'B') => StorageClass::CONST,
            Some(b'C') => StorageClass::VOLATILE,
            Some(b'D') => StorageClass::CONST | StorageClass::VOLATILE,
            Some(b'E') => StorageClass::FAR,
            Some(b'F') => StorageClass::CONST | StorageClass::FAR,
            Some(b'G') => StorageClass::VOLATILE | StorageClass::FAR,
            Some(b'H') => StorageClass::CONST | StorageClass::VOLATILE | StorageClass::FAR,
            _ => return StorageClass::empty(),
        };
        self.trim(1);
        storage_class
    }

    fn read_storage_class_for_return(&mut self) -> Result<StorageClass> {
        if !self.consume(b"?") {
            return Ok(StorageClass::empty());
        }
        let orig = self.input;

        Ok(match self.get()? {
            b'A' => StorageClass::empty(),
            b'B' => StorageClass::CONST,
            b'C' => StorageClass::VOLATILE,
            b'D' => StorageClass::CONST | StorageClass::VOLATILE,
            _ => {
                return Err(Error::new(format!(
                    "unknown storage class: {}",
                    str::from_utf8(orig)?
                )))
            }
        })
    }

    // Reads a variable type.
    fn read_var_type(&mut self, sc: StorageClass) -> Result<Type<'a>> {
        // println!("read_var_type on {}", str::from_utf8(self.input)?);
        if self.consume(b"W4") {
            let name = self.read_name(false)?;
            return Ok(Type::Enum(name, sc));
        }

        if self.consume(b"P6") {
            let _ = self.read_calling_conv()?; // TODO: keep around
            return self.read_func_ptr(sc);
        }

        if self.consume(b"P8") {
            let _name = self.read_unqualified_name(true);
            self.expect(b"@")?;
            let _is_64bit_ptr = self.expect(b"E")?;
            let access_class = self.read_func_access_class();
            let _calling_conv = self.read_calling_conv()?;
            let storage_class_for_return = self.read_storage_class_for_return()?;
            let return_type = self.read_func_return_type(storage_class_for_return)?;
            let params = self.read_params()?;
            return Ok(Type::Ptr(
                Box::new(Type::MemberFunction(
                    params,
                    access_class,
                    Box::new(return_type),
                )),
                sc,
            ));
        }

        if self.consume(b"$") {
            if self.consume(b"0") {
                let n = self.read_number()?;
                return Ok(Type::Constant(n));
            }
            if self.consume(b"D") {
                let n = self.read_number()?;
                return Ok(Type::TemplateParameterWithIndex(n));
            }
        }

        if self.consume(b"?") {
            let n = self.read_number()?;
            return Ok(Type::TemplateParameterWithIndex(-n));
        }

        if let Some(n) = self.consume_digit() {
            if n as usize >= self.memorized_types.len() {
                // println!("current memorized types: {:?}", self.memorized_types);
                return Err(Error::new(format!("invalid backreference: {}", n)));
            }

            return Ok(self.memorized_types[n as usize].clone());
        }

        let orig = self.input;

        Ok(match self.get()? {
            b'T' => Type::Union(self.read_name(false)?, sc),
            b'U' => Type::Struct(self.read_name(false)?, sc),
            b'V' => Type::Class(self.read_name(false)?, sc),
            b'A' => Type::Ref(Box::new(self.read_pointee()?), sc),
            b'B' => Type::Ref(Box::new(self.read_pointee()?), StorageClass::VOLATILE),
            b'P' => Type::Ptr(Box::new(self.read_pointee()?), sc),
            b'Q' => Type::Ptr(Box::new(self.read_pointee()?), StorageClass::CONST),
            b'R' => Type::Ptr(Box::new(self.read_pointee()?), StorageClass::VOLATILE),
            b'S' => Type::Ptr(
                Box::new(self.read_pointee()?),
                StorageClass::CONST | StorageClass::VOLATILE,
            ),
            b'Y' => self.read_array()?,
            b'X' => Type::Void(sc),
            b'D' => Type::Char(sc),
            b'C' => Type::Schar(sc),
            b'E' => Type::Uchar(sc),
            b'F' => Type::Short(sc),
            b'G' => Type::Ushort(sc),
            b'H' => Type::Int(sc),
            b'I' => Type::Uint(sc),
            b'J' => Type::Long(sc),
            b'K' => Type::Ulong(sc),
            b'M' => Type::Float(sc),
            b'N' => Type::Double(sc),
            b'O' => Type::Ldouble(sc),
            b'_' => match self.get()? {
                b'N' => Type::Bool(sc),
                b'J' => Type::Int64(sc),
                b'K' => Type::Uint64(sc),
                b'W' => Type::Wchar(sc),
                _ => {
                    return Err(Error::new(format!(
                        "unknown primitive type: {}",
                        str::from_utf8(orig)?
                    )))
                }
            },
            _ => {
                return Err(Error::new(format!(
                    "unknown primitive type: {}",
                    str::from_utf8(orig)?
                )))
            }
        })
    }

    fn read_pointee(&mut self) -> Result<Type<'a>> {
        let _is_64bit_ptr = self.expect(b"E");
        let storage_class = self.read_storage_class();
        self.read_var_type(storage_class)
    }

    fn read_array(&mut self) -> Result<Type<'a>> {
        let dimension = self.read_number()?;
        if dimension <= 0 {
            return Err(Error::new(format!(
                "invalid array dimension: {}",
                dimension
            )));
        }
        let (array, _) = self.read_nested_array(dimension)?;
        Ok(array)
    }

    fn read_nested_array(&mut self, dimension: i32) -> Result<(Type<'a>, StorageClass)> {
        if dimension > 0 {
            let len = self.read_number()?;
            let (inner_array, storage_class) = self.read_nested_array(dimension - 1)?;
            Ok((
                Type::Array(len, Box::new(inner_array), storage_class),
                storage_class,
            ))
        } else {
            let storage_class = if self.consume(b"$$C") {
                if self.consume(b"B") {
                    StorageClass::CONST
                } else if self.consume(b"C") || self.consume(b"D") {
                    StorageClass::CONST | StorageClass::VOLATILE
                } else if !self.consume(b"A") {
                    return Err(Error::new(format!(
                        "unknown storage class: {}",
                        str::from_utf8(self.input)?
                    )));
                } else {
                    StorageClass::empty()
                }
            } else {
                StorageClass::empty()
            };

            Ok((self.read_var_type(StorageClass::empty())?, storage_class))
        }
    }

    // Reads a function or a template parameters.
    fn read_params(&mut self) -> Result<Params<'a>> {
        // println!("read_params on {}", str::from_utf8(self.input)?);
        // Within the same parameter list, you can backreference the first 10 types.
        // let mut backref: Vec<Type<'a>> = Vec::with_capacity(10);

        let mut params: Vec<Type<'a>> = Vec::new();

        while !self.input.starts_with(b"@") && !self.input.starts_with(b"Z")
            && !self.input.is_empty()
        {
            if let Some(n) = self.consume_digit() {
                if n as usize >= self.memorized_types.len() {
                    return Err(Error::new(format!("invalid backreference: {}", n)));
                }
                // println!("reading a type from memorized_types[{}]. full list: {:#?}", n, self.memorized_types);
                params.push(self.memorized_types[n as usize].clone());
                continue;
            }

            let len = self.input.len();

            let param_type = self.read_var_type(StorageClass::empty())?;

            // Single-letter types are ignored for backreferences because
            // memorizing them doesn't save anything.
            if len - self.input.len() > 1 {
                self.memorize_type(&param_type);
            }
            params.push(param_type);
        }
        if self.input.starts_with(b"ZZ") {
            params.push(Type::VarArgs);
        }
        Ok(Params { types: params })
    }
}

pub fn demangle<'a>(input: &'a str, flags: DemangleFlags) -> Result<String> {
    serialize(parse(input)?, flags)
}

pub fn parse<'a>(input: &'a str) -> Result<ParseResult> {
    let state = ParserState {
        input: input.as_bytes(),
        memorized_names: Vec::with_capacity(10),
        memorized_types: Vec::with_capacity(10),
    };
    state.parse()
}

pub fn serialize(input: ParseResult, flags: DemangleFlags) -> Result<String> {
    let mut s = Vec::new();
    {
        let mut serializer = Serializer { flags, w: &mut s };
        serializer.serialize(&input).unwrap();
    }
    Ok(String::from_utf8(s)?)

}

// Converts an AST to a string.
//
// Converting an AST representing a C++ type to a string is tricky due
// to the bad grammar of the C++ declaration inherited from C. You have
// to construct a string from inside to outside. For example, if a type
// X is a pointer to a function returning int, the order you create a
// string becomes something like this:
//
//   (1) X is a pointer: *X
//   (2) (1) is a function returning int: int (*X)()
//
// So you cannot construct a result just by appending strings to a result.
//
// To deal with this, we split the function into two. write_pre() writes
// the "first half" of type declaration, and write_post() writes the
// "second half". For example, write_pre() writes a return type for a
// function and write_post() writes an parameter list.
struct Serializer<'a> {
    flags: DemangleFlags,
    w: &'a mut Vec<u8>,
}

impl<'a> Serializer<'a> {
    fn serialize(&mut self, parse_result: &ParseResult) -> SerializeResult<()> {
        self.write_pre(&parse_result.symbol_type)?;
        self.write_name(&parse_result.symbol)?;
        self.write_post(&parse_result.symbol_type)?;
        Ok(())
    }

    // Write the "first half" of a given type.
    fn write_pre(&mut self, t: &Type) -> SerializeResult<()> {
        let storage_class = match t {
            &Type::None => return Ok(()),
            &Type::MemberFunction(_, _, ref inner) => {
                self.write_pre(inner)?;
                return Ok(());
            }
            &Type::NonMemberFunction(_, _, ref inner) => {
                self.write_pre(inner)?;
                return Ok(());
            }
            &Type::CXXVBTable(_, sc) => sc,
            &Type::CXXVFTable(_, sc) => sc,
            &Type::TemplateParameterWithIndex(n) => {
                write!(self.w, "`template-parameter{}'", n)?;
                return Ok(());
            }
            &Type::Constant(n) => {
                write!(self.w, "{}", n)?;
                return Ok(());
            }
            &Type::VarArgs => {
                write!(self.w, "...")?;
                return Ok(());
            }
            &Type::Ptr(ref inner, storage_class) | &Type::Ref(ref inner, storage_class) => {
                self.write_pre(inner)?;

                // "[]" and "()" (for function parameters) take precedence over "*",
                // so "int *x(int)" means "x is a function returning int *". We need
                // parentheses to supercede the default precedence. (e.g. we want to
                // emit something like "int (*x)(int)".)
                match inner.as_ref() {
                    &Type::MemberFunction(_, _, _)
                    | &Type::NonMemberFunction(_, _, _)
                    | &Type::Array(_, _, _) => {
                        if self.flags == DemangleFlags::LotsOfWhitespace {
                            self.write_space()?;
                        }
                        write!(self.w, "(")?;
                    }
                    _ => {}
                }

                match t {
                    &Type::Ptr(_, _) => {
                        if self.flags == DemangleFlags::LotsOfWhitespace {
                            self.write_space()?;
                        }
                        write!(self.w, "*")?
                    }
                    &Type::Ref(_, _) => {
                        if self.flags == DemangleFlags::LotsOfWhitespace {
                            self.write_space()?;
                        }
                        write!(self.w, "&")?
                    }
                    _ => {}
                }

                storage_class
            }
            &Type::Array(_len, ref inner, storage_class) => {
                self.write_pre(inner)?;
                storage_class
            }
            &Type::Struct(ref names, sc) => {
                self.write_class(names, "struct")?;
                sc
            }
            &Type::Union(ref names, sc) => {
                self.write_class(names, "union")?;
                sc
            }
            &Type::Class(ref names, sc) => {
                self.write_class(names, "class")?;
                sc
            }
            &Type::Enum(ref names, sc) => {
                self.write_class(names, "enum")?;
                sc
            }
            &Type::Void(sc) => {
                write!(self.w, "void")?;
                sc
            }
            &Type::Bool(sc) => {
                write!(self.w, "bool")?;
                sc
            }
            &Type::Char(sc) => {
                write!(self.w, "char")?;
                sc
            }
            &Type::Schar(sc) => {
                write!(self.w, "signed char")?;
                sc
            }
            &Type::Uchar(sc) => {
                write!(self.w, "unsigned char")?;
                sc
            }
            &Type::Short(sc) => {
                write!(self.w, "short")?;
                sc
            }
            &Type::Ushort(sc) => {
                write!(self.w, "unsigned short")?;
                sc
            }
            &Type::Int(sc) => {
                write!(self.w, "int")?;
                sc
            }
            &Type::Uint(sc) => {
                write!(self.w, "unsigned int")?;
                sc
            }
            &Type::Long(sc) => {
                write!(self.w, "long")?;
                sc
            }
            &Type::Ulong(sc) => {
                write!(self.w, "unsigned long")?;
                sc
            }
            &Type::Int64(sc) => {
                write!(self.w, "int64_t")?;
                sc
            }
            &Type::Uint64(sc) => {
                write!(self.w, "uint64_t")?;
                sc
            }
            &Type::Wchar(sc) => {
                write!(self.w, "wchar_t")?;
                sc
            }
            &Type::Float(sc) => {
                write!(self.w, "float")?;
                sc
            }
            &Type::Double(sc) => {
                write!(self.w, "double")?;
                sc
            }
            &Type::Ldouble(sc) => {
                write!(self.w, "long double")?;
                sc
            }
        };

        if storage_class.contains(StorageClass::CONST) {
            self.write_space()?;
            write!(self.w, "const")?;
        }
        if storage_class.contains(StorageClass::VOLATILE) {
            self.write_space()?;
            write!(self.w, "volatile")?;
        }

        Ok(())
    }

    // Write the "second half" of a given type.
    fn write_post(&mut self, t: &Type) -> SerializeResult<()> {
        match t {
            &Type::MemberFunction(ref params, sc, ref return_type)
            | &Type::NonMemberFunction(ref params, sc, ref return_type) => {
                write!(self.w, "(")?;
                self.write_params(params)?;
                write!(self.w, ")")?;

                self.write_post(return_type)?;

                if sc.contains(StorageClass::CONST) {
                    write!(self.w, "const")?;
                    if self.flags == DemangleFlags::LotsOfWhitespace {
                        self.write_space()?;
                    }
                }
            }
            &Type::CXXVBTable(ref names, _sc) => {
                self.write_name(names)?;
                write!(self.w, "{}", "\'}")?; // the rest of the "operator"
            }
            &Type::Ptr(ref inner, _sc) | &Type::Ref(ref inner, _sc) => {
                match inner.as_ref() {
                    &Type::MemberFunction(_, _, _)
                    | &Type::NonMemberFunction(_, _, _)
                    | &Type::Array(_, _, _) => {
                        write!(self.w, ")")?;
                    }
                    _ => {}
                }
                self.write_post(inner)?;
            }
            &Type::Array(len, ref inner, _sc) => {
                write!(self.w, "[{}]", len)?;
                self.write_post(inner)?;
            }
            _ => {}
        }
        Ok(())
    }

    // Write a function or template parameter list.
    fn write_params(&mut self, p: &Params) -> SerializeResult<()> {
        for param in p.types.iter().take(p.types.len() - 1) {
            self.write_pre(param)?;
            self.write_post(param)?;
            write!(self.w, ",")?;
        }
        if let Some(param) = p.types.last() {
            self.write_pre(param)?;
            self.write_post(param)?;
        }
        Ok(())
    }

    fn write_class(&mut self, names: &NameSequence, s: &str) -> SerializeResult<()> {
        write!(self.w, "{}", s)?;
        write!(self.w, " ")?;
        self.write_name(names)?;
        Ok(())
    }

    fn write_space_pre(&mut self) -> SerializeResult<()> {
        if let Some(&c) = self.w.last() {
            match self.flags {
                DemangleFlags::LessWhitespace => {
                    if char::from(c).is_ascii_alphabetic() {
                        write!(self.w, " ")?;
                    }
                }
                DemangleFlags::LotsOfWhitespace => {
                    if char::from(c).is_ascii_alphabetic() || c == b'&' || c == b'>' {
                        write!(self.w, " ")?;
                    }
                }
            }
        }
        Ok(())
    }
    fn write_space(&mut self) -> SerializeResult<()> {
        if let Some(&c) = self.w.last() {
            match self.flags {
                DemangleFlags::LessWhitespace => {
                    if char::from(c).is_ascii_alphabetic() {
                        write!(self.w, " ")?;
                    }
                }
                DemangleFlags::LotsOfWhitespace => {
                    if char::from(c).is_ascii_alphabetic() || c == b'*' || c == b'&' || c == b'>' {
                        write!(self.w, " ")?;
                    }
                }
            }
        }
        Ok(())
    }

    fn write_one_name(&mut self, name: &Name) -> SerializeResult<()> {
        match name {
            &Name::Operator(op) => {
                match op {
                    _ => {
                        if self.flags == DemangleFlags::LotsOfWhitespace {
                            self.write_space()?;
                        }
                        // Print out an overloaded operator.
                        write!(self.w, "operator{}", op)?;
                    }
                }
                //panic!("only the last name should be an operator");
            }
            &Name::NonTemplate(ref name) => {
                self.w.write(name)?;
            }
            &Name::Template(ref name, ref params) => {
                self.write_one_name(name)?;
                self.write_tmpl_params(&params)?;
            }
        }
        Ok(())
    }

    // Write a name read by read_name().
    fn write_name(&mut self, names: &NameSequence) -> SerializeResult<()> {
        self.write_space_pre()?;

        // Print out namespaces or outer class names.
        for name in names.names.iter().rev().take(names.names.len() - 1) {
            self.write_one_name(&name)?;
            write!(self.w, "::")?;
        }

        if let Some(name) = names.names.first() {
            match name {
                &Name::Operator(op) => {
                    match op {
                        "ctor" => {
                            let prev = names.names.iter().nth(1).expect(
                                "If there's a ctor, there should be another name in this sequence",
                            );
                            self.write_one_name(prev)?;
                        }
                        "dtor" => {
                            let prev = names.names.iter().nth(1).expect(
                                "If there's a dtor, there should be another name in this sequence",
                            );
                            write!(self.w, "~")?;
                            self.write_one_name(prev)?;
                        }
                        "vbtable" => {
                            write!(self.w, "{}", "`vbtable'{for `")?;
                            // The rest will be written by write_post of the
                            // symbol type.
                        }
                        _ => {
                            if self.flags == DemangleFlags::LotsOfWhitespace {
                                self.write_space()?;
                            }
                            // Print out an overloaded operator.
                            write!(self.w, "operator{}", op)?;
                        }
                    }
                }
                &Name::NonTemplate(ref name) => {
                    self.w.write(name)?;
                }
                &Name::Template(ref name, ref params) => {
                    self.write_one_name(name)?;
                    self.write_tmpl_params(&params)?;
                }
            }
        }
        Ok(())
    }

    fn write_tmpl_params<'b>(&mut self, params: &Params<'b>) -> SerializeResult<()> {
        write!(self.w, "<")?;
        self.write_params(params)?;
        if let Some(&b'>') = self.w.last() {
            write!(self.w, " ")?;
        }
        write!(self.w, ">")?;
        Ok(())
    }
}

// grammar from MicrosoftMangle.cpp:

// <mangled-name> ::= ? <name> <type-encoding>
// <name> ::= <unscoped-name> {[<named-scope>]+ | [<nested-name>]}? @
// <unqualified-name> ::= <operator-name>
//                    ::= <ctor-dtor-name>
//                    ::= <source-name>
//                    ::= <template-name>
// <operator-name> ::= ???
//                 ::= ?B # cast, the target type is encoded as the return type.
// <source-name> ::= <identifier> @
//
// mangleNestedName: calls into mangle, which is responsible for <mangled-name>, and into mangleUnqualifiedName
// <postfix> ::= <unqualified-name> [<postfix>]
//           ::= <substitution> [<postfix>]
//
// <template-name> ::= <unscoped-template-name> <template-args>
//                 ::= <substitution>
// <unscoped-template-name> ::= ?$ <unqualified-name>
// <type-encoding> ::= <function-class> <function-type>
//                 ::= <storage-class> <variable-type>
// <function-class>  ::= <member-function> E? # E designates a 64-bit 'this'
//                                            # pointer. in 64-bit mode *all*
//                                            # 'this' pointers are 64-bit.
//                   ::= <global-function>
// <function-type> ::= <this-cvr-qualifiers> <calling-convention>
//                     <return-type> <argument-list> <throw-spec>
// <member-function> ::= A # private: near
//                   ::= B # private: far
//                   ::= C # private: static near
//                   ::= D # private: static far
//                   ::= E # private: near
//                   ::= F # private: far
//                   ::= I # near
//                   ::= J # far
//                   ::= K # static near
//                   ::= L # static far
//                   ::= M # near
//                   ::= N # far
//                   ::= Q # near
//                   ::= R # far
//                   ::= S # static near
//                   ::= T # static far
//                   ::= U # near
//                   ::= V # far
// <global-function> ::= Y # global near
//                   ::= Z # global far
// <storage-class> ::= 0  # private static member
//                 ::= 1  # protected static member
//                 ::= 2  # public static member
//                 ::= 3  # global
//                 ::= 4  # static local

#[cfg(test)]
mod tests {
    fn expect_with_flags(input: &str, reference: &str, flags: ::DemangleFlags) {
        let demangled: ::Result<_> = ::demangle(input, flags);
        let reference: ::Result<_> = Ok(reference.to_owned());
        assert_eq!(demangled, reference);
    }

    // std::basic_filebuf<char,struct std::char_traits<char> >::basic_filebuf<char,struct std::char_traits<char> >
    // std::basic_filebuf<char,struct std::char_traits<char> >::"operator ctor"
    // "operator ctor" = ?0

    #[test]
    fn other_tests() {
        let expect = |input, reference| {
            expect_with_flags(input, reference, ::DemangleFlags::LotsOfWhitespace);
        };

        expect("?f@@YAHQBH@Z", "int f(int const * const)");
        expect("?g@@YAHQAY0EA@$$CBH@Z", "int g(int const (* const)[64])");
        expect(
            "??0Klass@std@@AEAA@AEBV01@@Z",
            "std::Klass::Klass(class std::Klass const &)",
        );
        expect("??0?$Klass@V?$Mass@_N@@@std@@QEAA@AEBV01@@Z",
               "std::Klass<class Mass<bool> >::Klass<class Mass<bool> >(class std::Klass<class Mass<bool> > const &)");
    }

    #[test]
    fn upstream_tests() {
        let expect = |input, reference| {
            expect_with_flags(input, reference, ::DemangleFlags::LessWhitespace);
        };
        expect("?x@@3HA", "int x");
        expect("?x@@3PEAHEA", "int*x");
        expect("?x@@3PEAPEAHEA", "int**x");
        expect("?x@@3PEAY02HEA", "int(*x)[3]");
        expect("?x@@3PEAY124HEA", "int(*x)[3][5]");
        expect("?x@@3PEAY02$$CBHEA", "int const(*x)[3]");
        expect("?x@@3PEAEEA", "unsigned char*x");
        expect("?x@@3PEAY1NKM@5HEA", "int(*x)[3500][6]");
        expect("?x@@YAXMH@Z", "void x(float,int)");
        expect("?x@@YAXMH@Z", "void x(float,int)");
        expect("?x@@3P6AHMNH@ZEA", "int(*x)(float,double,int)");
        expect("?x@@3P6AHP6AHM@ZN@ZEA", "int(*x)(int(*)(float),double)");
        expect(
            "?x@@3P6AHP6AHM@Z0@ZEA",
            "int(*x)(int(*)(float),int(*)(float))",
        );

        expect("?x@ns@@3HA", "int ns::x");

        // Microsoft's undname returns "int const * const x" for this symbol.
        // I believe it's their bug.
        expect("?x@@3PEBHEB", "int const*x");

        expect("?x@@3QEAHEB", "int*const x");
        expect("?x@@3QEBHEB", "int const*const x");

        expect("?x@@3AEBHEB", "int const&x");

        expect("?x@@3PEAUty@@EA", "struct ty*x");
        expect("?x@@3PEATty@@EA", "union ty*x");
        expect("?x@@3PEAUty@@EA", "struct ty*x");
        expect("?x@@3PEAW4ty@@EA", "enum ty*x");
        expect("?x@@3PEAVty@@EA", "class ty*x");

        expect("?x@@3PEAV?$tmpl@H@@EA", "class tmpl<int>*x");
        expect("?x@@3PEAU?$tmpl@H@@EA", "struct tmpl<int>*x");
        expect("?x@@3PEAT?$tmpl@H@@EA", "union tmpl<int>*x");
        expect("?instance@@3Vklass@@A", "class klass instance");
        expect(
            "?instance$initializer$@@3P6AXXZEA",
            "void(*instance$initializer$)(void)",
        );
        expect("??0klass@@QEAA@XZ", "klass::klass(void)");
        expect("??1klass@@QEAA@XZ", "klass::~klass(void)");
        expect(
            "?x@@YAHPEAVklass@@AEAV1@@Z",
            "int x(class klass*,class klass&)",
        );
        expect(
            "?x@ns@@3PEAV?$klass@HH@1@EA",
            "class ns::klass<int,int>*ns::x",
        );
        expect(
            "?fn@?$klass@H@ns@@QEBAIXZ",
            "unsigned int ns::klass<int>::fn(void)const",
        );

        expect(
            "??4klass@@QEAAAEBV0@AEBV0@@Z",
            "class klass const&klass::operator=(class klass const&)",
        );
        expect("??7klass@@QEAA_NXZ", "bool klass::operator!(void)");
        expect(
            "??8klass@@QEAA_NAEBV0@@Z",
            "bool klass::operator==(class klass const&)",
        );
        expect(
            "??9klass@@QEAA_NAEBV0@@Z",
            "bool klass::operator!=(class klass const&)",
        );
        expect("??Aklass@@QEAAH_K@Z", "int klass::operator[](uint64_t)");
        expect("??Cklass@@QEAAHXZ", "int klass::operator->(void)");
        expect("??Dklass@@QEAAHXZ", "int klass::operator*(void)");
        expect("??Eklass@@QEAAHXZ", "int klass::operator++(void)");
        expect("??Eklass@@QEAAHH@Z", "int klass::operator++(int)");
        expect("??Fklass@@QEAAHXZ", "int klass::operator--(void)");
        expect("??Fklass@@QEAAHH@Z", "int klass::operator--(int)");
        expect("??Hklass@@QEAAHH@Z", "int klass::operator+(int)");
        expect("??Gklass@@QEAAHH@Z", "int klass::operator-(int)");
        expect("??Iklass@@QEAAHH@Z", "int klass::operator&(int)");
        expect("??Jklass@@QEAAHH@Z", "int klass::operator->*(int)");
        expect("??Kklass@@QEAAHH@Z", "int klass::operator/(int)");
        expect("??Mklass@@QEAAHH@Z", "int klass::operator<(int)");
        expect("??Nklass@@QEAAHH@Z", "int klass::operator<=(int)");
        expect("??Oklass@@QEAAHH@Z", "int klass::operator>(int)");
        expect("??Pklass@@QEAAHH@Z", "int klass::operator>=(int)");
        expect("??Qklass@@QEAAHH@Z", "int klass::operator,(int)");
        expect("??Rklass@@QEAAHH@Z", "int klass::operator()(int)");
        expect("??Sklass@@QEAAHXZ", "int klass::operator~(void)");
        expect("??Tklass@@QEAAHH@Z", "int klass::operator^(int)");
        expect("??Uklass@@QEAAHH@Z", "int klass::operator|(int)");
        expect("??Vklass@@QEAAHH@Z", "int klass::operator&&(int)");
        expect("??Wklass@@QEAAHH@Z", "int klass::operator||(int)");
        expect("??Xklass@@QEAAHH@Z", "int klass::operator*=(int)");
        expect("??Yklass@@QEAAHH@Z", "int klass::operator+=(int)");
        expect("??Zklass@@QEAAHH@Z", "int klass::operator-=(int)");
        expect("??_0klass@@QEAAHH@Z", "int klass::operator/=(int)");
        expect("??_1klass@@QEAAHH@Z", "int klass::operator%=(int)");
        expect("??_2klass@@QEAAHH@Z", "int klass::operator>>=(int)");
        expect("??_3klass@@QEAAHH@Z", "int klass::operator<<=(int)");
        expect("??_6klass@@QEAAHH@Z", "int klass::operator^=(int)");
        expect(
            "??6@YAAEBVklass@@AEBV0@H@Z",
            "class klass const&operator<<(class klass const&,int)",
        );
        expect(
            "??5@YAAEBVklass@@AEBV0@_K@Z",
            "class klass const&operator>>(class klass const&,uint64_t)",
        );
        expect(
            "??2@YAPEAX_KAEAVklass@@@Z",
            "void*operator new(uint64_t,class klass&)",
        );
        expect(
            "??_U@YAPEAX_KAEAVklass@@@Z",
            "void*operator new[](uint64_t,class klass&)",
        );
        expect(
            "??3@YAXPEAXAEAVklass@@@Z",
            "void operator delete(void*,class klass&)",
        );
        expect(
            "??_V@YAXPEAXAEAVklass@@@Z",
            "void operator delete[](void*,class klass&)",
        );
    }
}
