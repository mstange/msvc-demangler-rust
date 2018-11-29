// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
// This file defines a demangler for MSVC-style mangled symbols.

#[macro_use]
extern crate bitflags;

use std::cmp::min;
use std::io::Write;
use std::mem;
use std::result;
use std::str;

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
        const CONST       = 0b00000001;
        const VOLATILE    = 0b00000010;
        const FAR         = 0b00000100;
        const HUGE        = 0b00001000;
        const UNALIGNED   = 0b00010000;
        const RESTRICT    = 0b00100000;
        const LVALUE_QUAL = 0b01000000;
        const RVALUE_QUAL = 0b10000000;
    }
}

bitflags! {
    pub struct DemangleFlags: u32 {
        /// Undecorate 32-bit decorated names.
        const DECODE_32_BIT = 0x0800;
        /// Enable full undecoration.
        const COMPLETE = 0x0000;
        /// Undecorate only the name for primary declaration. Returns [scope::]name. Does expand template parameters.
        const NAME_ONLY = 0x1000;
        /// Disable expansion of access specifiers for members.
        const NO_ACCESS_SPECIFIERS = 0x0080;
        // /// Disable expansion of the declaration language specifier.
        // const NO_ALLOCATION_LANGUAGE = 0x0010;
        // /// Disable expansion of the declaration model.
        // const NO_ALLOCATION_MODEL = 0x0008;
        // /// Do not undecorate function arguments.
        // const NO_ARGUMENTS = 0x2000;
        /// Disable expansion of CodeView modifiers on the this type for primary declaration.
        const NO_CV_THISTYPE = 0x0040;
        /// Disable expansion of return types for primary declarations.
        const NO_FUNCTION_RETURNS = 0x0004;
        // /// Remove leading underscores from Microsoft keywords.
        // const NO_LEADING_UNDERSCORES = 0x0001;
        /// Disable expansion of the static or virtual attribute of members.
        const NO_MEMBER_TYPE = 0x0200;
        /// Disable expansion of Microsoft keywords.
        const NO_MS_KEYWORDS = 0x0002;
        /// Disable expansion of Microsoft keywords on the this type for primary declaration.
        const NO_MS_THISTYPE = 0x0020;
        // /// Disable expansion of the Microsoft model for user-defined type returns.
        // const NO_RETURN_UDT_MODEL = 0x0400;
        // /// Do not undecorate special names, such as vtable, vcall, vector, metatype, and so on.
        // const NO_SPECIAL_SYMS = 0x4000;
        /// Disable all modifiers on the this type.
        const NO_THISTYPE = Self::NO_MS_THISTYPE.bits | Self::NO_CV_THISTYPE.bits;
        // /// Disable expansion of throw-signatures for functions and pointers to functions.
        // const NO_THROW_SIGNATURES = 0x0100;
    }
}

// Calling conventions
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CallingConv {
    Cdecl,
    Pascal,
    Thiscall,
    Stdcall,
    Fastcall,
    _Regcall,
}

bitflags! {
    pub struct FuncClass: u32 {
        const PUBLIC     = 0b00000001;
        const PROTECTED  = 0b00000010;
        const PRIVATE    = 0b00000100;
        const GLOBAL     = 0b00001000;
        const STATIC     = 0b00010000;
        const VIRTUAL    = 0b00100000;
        const FAR        = 0b01000000;
        const THUNK      = 0b10000000;
    }
}

// Represents an identifier which may be a template.
#[derive(Clone, Debug, PartialEq)]
pub enum Name<'a> {
    Operator(Operator<'a>),
    NonTemplate(&'a [u8]),
    Template(Box<Name<'a>>, Params<'a>),
    Discriminator(i32),
    ParsedName(Box<ParseResult<'a>>),
    AnonymousNamespace,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator<'a> {
    Ctor,
    Dtor,
    New,
    Delete,
    Equal,
    RShift,
    LShift,
    Bang,
    EqualEqual,
    BangEqual,
    Subscript,
    Conversion, // TODO
    Arrow,
    Star,
    PlusPlus,
    MinusMinus,
    Minus,
    Plus,
    Amp,
    ArrowStar,
    Slash,
    Percent,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Comma,
    Call,
    Tilde,
    Caret,
    Pipe,
    AmpAmp,
    PipePipe,
    StarEqual,
    PlusEqual,
    MinusEqual,
    SlashEqual,
    PercentEqual,
    GreaterGreaterEqual,
    LessLessEqual,
    AmpEqual,
    PipeEqual,
    CaretEqual,

    VFTable,
    VBTable,
    VCall,
    Typeof,
    LocalStaticGuard,
    String,
    VBaseDtor,
    VectorDeletingDtor,
    DefaultCtorClosure,
    ScalarDeletingDtor,
    VectorCtorIterator,
    VectorDtorIterator,
    VectorVBaseCtorIterator,
    VirtualDisplacementMap,
    EHVectorCtorIterator,
    EHVectorDtorIterator,
    EHVectorVBaseCtorIterator,
    CopyCtorClosure,

    LocalVFTable,
    LocalVFTableCtorClosure,
    ArrayNew,
    ArrayDelete,
    PlacementDeleteClosure,
    PlacementArrayDeleteClosure,

    CoroutineAwait,
    LiteralOperatorName,

    RTTITypeDescriptor(StorageClass, Box<Type<'a>>),
    RTTIBaseClassDescriptor(i32, i32, i32, i32),
    RTTIBaseClassArray,
    RTTIClassHierarchyDescriptor,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NameSequence<'a> {
    pub names: Vec<Name<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Params<'a> {
    pub types: Vec<Type<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol<'a> {
    pub name: Name<'a>,
    pub scope: NameSequence<'a>,
}

// The type class. Mangled symbols are first parsed and converted to
// this type and then converted to string.
#[derive(Clone, Debug, PartialEq)]
pub enum Type<'a> {
    None,
    MemberFunction(
        FuncClass,
        CallingConv,
        Params<'a>,
        StorageClass,
        Box<Type<'a>>,
    ), // StorageClass is for the 'this' pointer
    MemberFunctionPointer(
        Symbol<'a>,
        FuncClass,
        CallingConv,
        Params<'a>,
        StorageClass,
        Box<Type<'a>>,
    ),
    NonMemberFunction(CallingConv, Params<'a>, StorageClass, Box<Type<'a>>),
    CXXVBTable(NameSequence<'a>, StorageClass),
    CXXVFTable(NameSequence<'a>, StorageClass),
    VCallThunk(i32, CallingConv),
    TemplateParameterWithIndex(i32),
    ThreadSafeStaticGuard(i32),
    Constant(i32),
    ConstantString(Vec<u8>),
    Ptr(Box<Type<'a>>, StorageClass),
    Ref(Box<Type<'a>>, StorageClass),
    RValueRef(Box<Type<'a>>, StorageClass),
    Array(i32, Box<Type<'a>>, StorageClass),

    Struct(Symbol<'a>, StorageClass),
    Union(Symbol<'a>, StorageClass),
    Class(Symbol<'a>, StorageClass),
    Enum(Symbol<'a>, StorageClass),

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
    Char16(StorageClass),
    Char32(StorageClass),
    Float(StorageClass),
    Double(StorageClass),
    Ldouble(StorageClass),
    VarArgs,
    EmptyParameterPack,
    Nullptr,
    RTTIType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseResult<'a> {
    pub symbol: Symbol<'a>,
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
    fn parse(&mut self) -> Result<ParseResult<'a>> {
        // MSVC-style mangled symbols must start with b'?'.
        if !self.consume(b"?") {
            return Err(Error::new("does not start with b'?'".to_owned()));
        }

        if self.consume(b"$") {
            if self.consume(b"TSS") {
                let mut guard_num: i32 =
                    self.consume_digit()
                        .ok_or(Error::new("missing digit".to_owned()))? as i32;
                while !self.consume(b"@") {
                    guard_num = guard_num * 10
                        + self
                            .consume_digit()
                            .ok_or(Error::new("missing digit".to_owned()))?
                            as i32;
                }
                let name = self.read_nested_name()?;
                let scope = self.read_scope()?;
                self.expect(b"4HA")?;
                return Ok(ParseResult {
                    symbol: Symbol { name, scope },
                    symbol_type: Type::ThreadSafeStaticGuard(guard_num),
                });
            }
            let name = self.read_template_name()?;
            return Ok(ParseResult {
                symbol: Symbol {
                    name,
                    scope: NameSequence { names: Vec::new() },
                },
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
                    let access_class = self.read_qualifier();
                    let scope = self.read_scope()?;
                    Type::CXXVFTable(scope, access_class)
                }
                b'7' => {
                    let access_class = self.read_qualifier();
                    let scope = self.read_scope()?;
                    Type::CXXVBTable(scope, access_class)
                }
                b'9' => {
                    // extern "C" names have their class and type omitted.
                    Type::None
                }
                b'Y' => {
                    // Read a non-member function.
                    let calling_conv = self.read_calling_conv()?;
                    let storage_class = self.read_storage_class_for_return()?;
                    let return_type = self.read_var_type(storage_class)?;
                    let params = self.read_func_params()?;
                    Type::NonMemberFunction(
                        calling_conv,
                        params,
                        StorageClass::empty(),
                        Box::new(return_type),
                    )
                }
                b'_' => {
                    // Read an encoded string.
                    let char_bytes = match self.get()? {
                        b'0' => 1, // char
                        b'1' => 2, // wchar_t
                        _ => {
                            return Err(Error::new("unknown string character type".to_owned()));
                        }
                    };
                    self.read_encoded_string(char_bytes)?
                }
                b'$' => {
                    self.expect(b"B")?;
                    let vftable_offset = self.read_number()?;
                    self.expect(b"A")?;
                    let calling_conv = self.read_calling_conv()?;
                    Type::VCallThunk(vftable_offset, calling_conv)
                }
                b'8' => Type::RTTIType,
                c => {
                    // Read a member function.
                    let func_class = self.read_func_class(c)?;
                    let access_class;
                    if func_class.contains(FuncClass::STATIC) {
                        access_class = StorageClass::empty();
                    } else {
                        let _is_64bit_ptr = self.expect(b"E");
                        let restrict = if self.consume(b"I") {
                            StorageClass::RESTRICT
                        } else {
                            StorageClass::empty()
                        };
                        let ref_qualifiers = match self.peek() {
                            Some(b'G') => {
                                self.expect(b"G").unwrap();
                                StorageClass::LVALUE_QUAL
                            }
                            Some(b'H') => {
                                self.expect(b"H").unwrap();
                                StorageClass::RVALUE_QUAL
                            }
                            _ => StorageClass::empty(),
                        };
                        access_class = self.read_qualifier() | restrict | ref_qualifiers;
                    }

                    let calling_conv = self.read_calling_conv()?;
                    let storage_class_for_return = self.read_storage_class_for_return()?;
                    let return_type = self.read_func_return_type(storage_class_for_return)?;
                    let params = self.read_func_params()?;
                    Type::MemberFunction(
                        func_class,
                        calling_conv,
                        params,
                        access_class,
                        Box::new(return_type),
                    )
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

    fn consume_hex_digit(&mut self) -> bool {
        match self.peek() {
            Some(first) => {
                if char::from(first).is_digit(16) {
                    self.trim(1);
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn read_encoded_string(&mut self, char_bytes: i32) -> Result<Type<'a>> {
        let byte_length = self.read_number()?; // including null terminator
        let _crc = self.read_number()?;
        let bytes = min(byte_length, char_bytes * 32);

        let mut collected = vec![];
        for _i in 0..bytes {
            let c = self.get()?;
            let byte: u8 = match c {
                b'0'...b'9' | b'a'...b'z' | b'A'...b'Z' | b'_' | b'$' => c,
                b'?' => {
                    let c = self.get()?;
                    match c {
                        b'A'...b'Z' => c - b'A' + 0xe1,
                        b'a'...b'z' => c - b'A' + 0xc1,
                        b'0'...b'9' => {
                            let v = &[
                                b',', b'/', b'\\', b':', b'.', b' ', b'\n', b'\t', b'\'', b'-',
                            ];
                            v[(c - b'0') as usize]
                        }
                        b'$' => {
                            let high = self.get()? - b'A';
                            let low = self.get()? - b'A';
                            high << 4 | low
                        }
                        _ => {
                            return Err(Error::new(format!(
                                "unknown escaped encoded string character {}",
                                char::from(c)
                            )));
                        }
                    }
                }
                _ => {
                    return Err(Error::new(format!(
                        "unknown escaped encoded string character {}",
                        char::from(c)
                    )));
                }
            };
            collected.push(byte);
        }

        Ok(Type::ConstantString(collected))
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
        Ok(Name::Template(Box::new(name), template_params))
    }

    fn read_nested_name(&mut self) -> Result<Name<'a>> {
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
        } else if self.consume(b"?") {
            match self.peek() {
                Some(b'?') => {
                    let name = Name::ParsedName(Box::new(self.parse()?));
                    // println!("parsed name: {}", str::from_utf8(self.input)?);
                    name
                }
                _ => {
                    if self.consume(b"$") {
                        let name = self.read_template_name()?;
                        self.memorize_name(&name);
                        name
                    } else if self.consume(b"A") {
                        // Anonymous namespace.
                        if self.consume(b"0x") {
                            while self.consume_hex_digit() {}
                        }
                        self.expect(b"@")?;
                        Name::AnonymousNamespace
                    } else {
                        let discriminator = self.read_number()?;
                        Name::Discriminator(discriminator)
                    }
                }
            }
        } else {
            // Non-template functions or classes.
            let name = self.read_string()?;
            let name = Name::NonTemplate(name);
            self.memorize_name(&name);
            name
        };
        Ok(name)
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

    fn read_scope(&mut self) -> Result<NameSequence<'a>> {
        let mut names = Vec::new();
        while !self.consume(b"@") {
            // println!("read_name iteration on {}", str::from_utf8(self.input)?);
            let name = self.read_nested_name()?;
            names.push(name);
        }
        Ok(NameSequence { names })
    }

    // Parses a name in the form of A@B@C@@ which represents C::B::A.
    fn read_name(&mut self, function: bool) -> Result<Symbol<'a>> {
        // println!("read_name on {}", str::from_utf8(self.input)?);
        let name = self.read_unqualified_name(function)?;
        let scope = self.read_scope()?;

        Ok(Symbol { name, scope })
    }

    fn read_func_type(&mut self) -> Result<Type<'a>> {
        let calling_conv = self.read_calling_conv()?;
        let return_type = self.read_var_type(StorageClass::empty())?;
        let params = self.read_func_params()?;
        return Ok(Type::NonMemberFunction(
            calling_conv,
            params,
            StorageClass::empty(),
            Box::new(return_type),
        ));
    }

    fn read_operator(&mut self) -> Result<Name<'a>> {
        Ok(Name::Operator(self.read_operator_name()?))
    }

    fn read_operator_name(&mut self) -> Result<Operator<'a>> {
        let orig = self.input;

        Ok(match self.get()? {
            b'0' => Operator::Ctor,
            b'1' => Operator::Dtor,
            b'2' => Operator::New,
            b'3' => Operator::Delete,
            b'4' => Operator::Equal,
            b'5' => Operator::RShift,
            b'6' => Operator::LShift,
            b'7' => Operator::Bang,
            b'8' => Operator::EqualEqual,
            b'9' => Operator::BangEqual,
            b'A' => Operator::Subscript,
            b'B' => Operator::Conversion, // TODO
            b'C' => Operator::Arrow,
            b'D' => Operator::Star,
            b'E' => Operator::PlusPlus,
            b'F' => Operator::MinusMinus,
            b'G' => Operator::Minus,
            b'H' => Operator::Plus,
            b'I' => Operator::Amp,
            b'J' => Operator::ArrowStar,
            b'K' => Operator::Slash,
            b'L' => Operator::Percent,
            b'M' => Operator::Less,
            b'N' => Operator::LessEqual,
            b'O' => Operator::Greater,
            b'P' => Operator::GreaterEqual,
            b'Q' => Operator::Comma,
            b'R' => Operator::Call,
            b'S' => Operator::Tilde,
            b'T' => Operator::Caret,
            b'U' => Operator::Pipe,
            b'V' => Operator::AmpAmp,
            b'W' => Operator::PipePipe,
            b'X' => Operator::StarEqual,
            b'Y' => Operator::PlusEqual,
            b'Z' => Operator::MinusEqual,
            b'_' => match self.get()? {
                b'0' => Operator::SlashEqual,
                b'1' => Operator::PercentEqual,
                b'2' => Operator::GreaterGreaterEqual,
                b'3' => Operator::LessLessEqual,
                b'4' => Operator::AmpEqual,
                b'5' => Operator::PipeEqual,
                b'6' => Operator::CaretEqual,
                b'7' => Operator::VFTable,
                b'8' => Operator::VBTable,
                b'9' => Operator::VCall,
                b'A' => Operator::Typeof,
                b'B' => Operator::LocalStaticGuard,
                b'C' => Operator::String,
                b'D' => Operator::VBaseDtor,
                b'E' => Operator::VectorDeletingDtor,
                b'F' => Operator::DefaultCtorClosure,
                b'G' => Operator::ScalarDeletingDtor,
                b'H' => Operator::VectorCtorIterator,
                b'I' => Operator::VectorDtorIterator,
                b'J' => Operator::VectorVBaseCtorIterator,
                b'K' => Operator::VirtualDisplacementMap,
                b'L' => Operator::EHVectorCtorIterator,
                b'M' => Operator::EHVectorDtorIterator,
                b'N' => Operator::EHVectorVBaseCtorIterator,
                b'O' => Operator::CopyCtorClosure,
                b'R' => {
                    let c = self.get()?;
                    match c {
                        b'0' => {
                            self.expect(b"?")?;
                            let storage_class = self.read_storage_class();
                            let t = self.read_var_type(storage_class)?;
                            Operator::RTTITypeDescriptor(storage_class, Box::new(t))
                        }
                        b'1' => {
                            let nv_offset = self.read_number()?;
                            let vbptr_offset = self.read_number()?;
                            let vbtable_offset = self.read_number()?;
                            let flags = self.read_number()?;
                            Operator::RTTIBaseClassDescriptor(
                                nv_offset,
                                vbptr_offset,
                                vbtable_offset,
                                flags,
                            )
                        }
                        b'2' => Operator::RTTIBaseClassArray,
                        b'3' => Operator::RTTIClassHierarchyDescriptor,
                        _ => {
                            return Err(Error::new(format!(
                                "unknown RTTI Operator name: {}",
                                str::from_utf8(&[c])?
                            )));
                        }
                    }
                }
                b'S' => Operator::LocalVFTable,
                b'T' => Operator::LocalVFTableCtorClosure,
                b'U' => Operator::ArrayNew,
                b'V' => Operator::ArrayDelete,
                b'X' => Operator::PlacementDeleteClosure,
                b'Y' => Operator::PlacementArrayDeleteClosure,
                b'_' => {
                    if self.consume(b"L") {
                        Operator::CoroutineAwait
                    } else if self.consume(b"K") {
                        Operator::LiteralOperatorName // TODO: read <source-name>, that's the operator name
                    } else {
                        return Err(Error::new(format!(
                            "unknown operator name: {}",
                            str::from_utf8(orig)?
                        )));
                    }
                }
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
        // TODO: need to figure out how to wrap up the adjustment.
        let mut read_thunk = |func_class| -> Result<FuncClass> {
            let _adjustment = self.read_number()?;
            Ok(func_class | FuncClass::THUNK)
        };

        Ok(match c {
            b'A' => FuncClass::PRIVATE,
            b'B' => FuncClass::PRIVATE | FuncClass::FAR,
            b'C' => FuncClass::PRIVATE | FuncClass::STATIC,
            b'D' => FuncClass::PRIVATE | FuncClass::STATIC,
            b'E' => FuncClass::PRIVATE | FuncClass::VIRTUAL,
            b'F' => FuncClass::PRIVATE | FuncClass::VIRTUAL,
            b'G' => read_thunk(FuncClass::PRIVATE | FuncClass::VIRTUAL)?,
            b'H' => read_thunk(FuncClass::PRIVATE | FuncClass::VIRTUAL | FuncClass::FAR)?,
            b'I' => FuncClass::PROTECTED,
            b'J' => FuncClass::PROTECTED | FuncClass::FAR,
            b'K' => FuncClass::PROTECTED | FuncClass::STATIC,
            b'L' => FuncClass::PROTECTED | FuncClass::STATIC | FuncClass::FAR,
            b'M' => FuncClass::PROTECTED | FuncClass::VIRTUAL,
            b'N' => FuncClass::PROTECTED | FuncClass::VIRTUAL | FuncClass::FAR,
            b'O' => read_thunk(FuncClass::PROTECTED | FuncClass::VIRTUAL)?,
            b'P' => read_thunk(FuncClass::PROTECTED | FuncClass::VIRTUAL | FuncClass::FAR)?,
            b'Q' => FuncClass::PUBLIC,
            b'R' => FuncClass::PUBLIC | FuncClass::FAR,
            b'S' => FuncClass::PUBLIC | FuncClass::STATIC,
            b'T' => FuncClass::PUBLIC | FuncClass::STATIC | FuncClass::FAR,
            b'U' => FuncClass::PUBLIC | FuncClass::VIRTUAL,
            b'V' => FuncClass::PUBLIC | FuncClass::VIRTUAL | FuncClass::FAR,
            b'W' => read_thunk(FuncClass::PUBLIC | FuncClass::VIRTUAL)?,
            b'X' => read_thunk(FuncClass::PUBLIC | FuncClass::VIRTUAL | FuncClass::FAR)?,
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

    fn read_qualifier(&mut self) -> StorageClass {
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

    fn read_member_function_pointer(&mut self, read_qualifiers: bool) -> Result<Type<'a>> {
        let symbol = self.read_name(true)?;
        let _is_64bit_ptr = self.consume(b"E");
        let (access_class, func_class) = if read_qualifiers {
            (self.read_qualifier(), FuncClass::empty())
        } else {
            let c = self.get()?;
            (StorageClass::empty(), self.read_func_class(c)?)
        };
        let calling_conv = self.read_calling_conv()?;
        let storage_class_for_return = self.read_storage_class_for_return()?;
        let return_type = self.read_func_return_type(storage_class_for_return)?;
        let params = self.read_func_params()?;
        Ok(Type::MemberFunctionPointer(
            symbol,
            func_class,
            calling_conv,
            params,
            access_class,
            Box::new(return_type),
        ))
    }

    // Reads a variable type.
    fn read_var_type(&mut self, mut sc: StorageClass) -> Result<Type<'a>> {
        // println!("read_var_type on {}", str::from_utf8(self.input)?);
        if self.consume(b"W4") {
            let name = self.read_name(false)?;
            return Ok(Type::Enum(name, sc));
        }

        if self.consume(b"A6") {
            let func_type = self.read_func_type()?;
            return Ok(Type::Ref(Box::new(func_type), sc));
        }

        if self.consume(b"P6") {
            let func_type = self.read_func_type()?;
            return Ok(Type::Ptr(Box::new(func_type), sc));
        }

        if self.consume(b"P8") {
            return self.read_member_function_pointer(true);
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
            if self.consume(b"$BY") {
                return Ok(self.read_array()?);
            }
            if self.consume(b"$Q") {
                return Ok(Type::RValueRef(Box::new(self.read_pointee()?), sc));
            }
            if self.consume(b"$V") {
                return Ok(Type::EmptyParameterPack);
            }
            if self.consume(b"$T") {
                return Ok(Type::Nullptr);
            }
            if self.consume(b"$A6") {
                return self.read_func_type();
            }
            // These next cases can fallthrough, so be careful adding new ones!
            if self.consume(b"$C") {
                sc = self.read_qualifier();
            } else if let Some(x) = self.peek() {
                match x {
                    // Inheritance specifiers, which we don't need to remember.
                    b'1' | b'H' | b'I' | b'J' => {
                        self.trim(1);
                        self.expect(b"?")?;
                        return self.read_member_function_pointer(false);
                    }
                    _ => {}
                };
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
                b'S' => Type::Char16(sc),
                b'U' => Type::Char32(sc),
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

        while !self.input.starts_with(b"@")
            && !self.input.starts_with(b"Z")
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

        if self.consume(b"Z") {
            params.push(Type::VarArgs);
        } else if self.input.is_empty() {
            // this is needed to handle the weird standalone template manglings
        } else {
            self.expect(b"@")?;
        }
        Ok(Params { types: params })
    }

    // Reads a function parameters.
    fn read_func_params(&mut self) -> Result<Params<'a>> {
        let params = if self.consume(b"X") {
            Params {
                types: vec![Type::Void(StorageClass::empty())],
            }
        } else {
            self.read_params()?
        };

        self.expect(b"Z")?;

        Ok(params)
    }
}

pub fn demangle<'a>(input: &'a str, flags: DemangleFlags) -> Result<String> {
    serialize(&parse(input)?, flags)
}

pub fn parse<'a>(input: &'a str) -> Result<ParseResult> {
    let mut state = ParserState {
        input: input.as_bytes(),
        memorized_names: Vec::with_capacity(10),
        memorized_types: Vec::with_capacity(10),
    };
    state.parse()
}

pub fn serialize(input: &ParseResult, flags: DemangleFlags) -> Result<String> {
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
        if !self.flags.intersects(DemangleFlags::NAME_ONLY | DemangleFlags::NO_FUNCTION_RETURNS) {
            self.write_pre(&parse_result.symbol_type)?;
        }
        self.write_name(&parse_result.symbol)?;
        if !self.flags.contains(DemangleFlags::NAME_ONLY) {
            self.write_post(&parse_result.symbol_type)?;
        }
        Ok(())
    }

    fn write_calling_conv(&mut self, calling_conv: CallingConv) -> SerializeResult<()> {
        if let Some(&b' ') = self.w.last() {
        } else {
            write!(self.w, " ")?;
        }
        if !self.flags.contains(DemangleFlags::NO_MS_KEYWORDS) {
            match calling_conv {
                CallingConv::Cdecl => {
                    write!(self.w, "__cdecl ")?;
                }
                CallingConv::Pascal => {}
                CallingConv::Thiscall => {
                    write!(self.w, "__thiscall ")?;
                }
                CallingConv::Stdcall => {
                    write!(self.w, "__stdcall ")?;
                }
                CallingConv::Fastcall => {
                    write!(self.w, "__fastcall ")?;
                }
                CallingConv::_Regcall => {
                    write!(self.w, "__regcall ")?;
                }
            };
        }

        Ok(())
    }

    // Write the "first half" of a given type.
    fn write_pre(&mut self, t: &Type) -> SerializeResult<()> {
        let storage_class = match t {
            &Type::None => return Ok(()),
            &Type::MemberFunction(func_class, calling_conv, _, _, ref inner) => {
                if func_class.contains(FuncClass::THUNK) {
                    write!(self.w, "[thunk]:")?
                }
                if !self.flags.contains(DemangleFlags::NO_ACCESS_SPECIFIERS) {
                    if func_class.contains(FuncClass::PRIVATE) {
                        write!(self.w, "private: ")?
                    }
                    if func_class.contains(FuncClass::PROTECTED) {
                        write!(self.w, "protected: ")?
                    }
                    if func_class.contains(FuncClass::PUBLIC) {
                        write!(self.w, "public: ")?
                    }
                }
                if !self.flags.contains(DemangleFlags::NO_MEMBER_TYPE) {
                    if func_class.contains(FuncClass::STATIC) {
                        write!(self.w, "static ")?
                    }
                    if func_class.contains(FuncClass::VIRTUAL) {
                        write!(self.w, "virtual ")?;
                    }
                }
                self.write_pre(inner)?;
                self.write_calling_conv(calling_conv)?;
                return Ok(());
            }
            &Type::MemberFunctionPointer(ref symbol, _, calling_conv, _, _, ref inner) => {
                self.write_pre(inner)?;
                self.write_calling_conv(calling_conv)?;
                self.write_space()?;
                write!(self.w, "(")?;
                self.write_space()?;
                self.write_name(symbol)?;
                write!(self.w, "::*)")?;
                return Ok(());
            }
            &Type::NonMemberFunction(calling_conv, _, _, ref inner) => {
                self.write_pre(inner)?;
                self.write_calling_conv(calling_conv)?;
                return Ok(());
            }
            &Type::VCallThunk(_, calling_conv) => {
                write!(self.w, "[thunk]: ")?;
                self.write_calling_conv(calling_conv)?;
                return Ok(());
            }
            &Type::CXXVBTable(_, sc) => sc,
            &Type::CXXVFTable(_, sc) => sc,
            &Type::TemplateParameterWithIndex(n) => {
                write!(self.w, "`template-parameter{}'", n)?;
                return Ok(());
            }
            &Type::ThreadSafeStaticGuard(num) => {
                write!(self.w, "TSS{}", num)?;
                return Ok(());
            }
            &Type::Constant(n) => {
                write!(self.w, "{}", n)?;
                return Ok(());
            }
            &Type::ConstantString(_) => {
                // We have no idea what the original encoding of the string is,
                // and undname doesn't even try to display anything.
                //match str::from_utf8(s).ok() {
                //  Some(ref s) => write!(self.w, "{}", s)?,
                //  None => {},
                //}
                return Ok(());
            }
            &Type::VarArgs => {
                write!(self.w, "...")?;
                return Ok(());
            }
            &Type::Ptr(ref inner, storage_class)
            | &Type::Ref(ref inner, storage_class)
            | &Type::RValueRef(ref inner, storage_class) => {
                self.write_pre(inner)?;

                // "[]" and "()" (for function parameters) take precedence over "*",
                // so "int *x(int)" means "x is a function returning int *". We need
                // parentheses to supercede the default precedence. (e.g. we want to
                // emit something like "int (*x)(int)".)
                match inner.as_ref() {
                    &Type::MemberFunction(_, _, _, _, _)
                    | &Type::NonMemberFunction(_, _, _, _)
                    | &Type::Array(_, _, _) => {
                        self.write_space()?;
                        write!(self.w, "(")?;
                    }
                    _ => {}
                }

                match t {
                    &Type::Ptr(_, _) => {
                        self.write_space()?;
                        write!(self.w, "*")?
                    }
                    &Type::Ref(_, _) => {
                        self.write_space()?;
                        write!(self.w, "&")?
                    }
                    &Type::RValueRef(_, _) => {
                        self.write_space()?;
                        write!(self.w, "&&")?
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
            &Type::Char16(sc) => {
                write!(self.w, "char16_t")?;
                sc
            }
            &Type::Char32(sc) => {
                write!(self.w, "char32_t")?;
                sc
            }
            &Type::Nullptr => {
                write!(self.w, "std::nullptr_t")?;
                return Ok(());
            }
            &Type::EmptyParameterPack => return Ok(()),
            &Type::RTTIType => return Ok(()),
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

    fn write_memfn_qualifiers(&mut self, sc: StorageClass) -> SerializeResult<()> {
        if self.flags.contains(DemangleFlags::NO_THISTYPE) {
            // TODO: should probably check for NO_CV_THISTYPE and NO_MS_THISTYPE
            // separately but I don't know what exactly those affect.
            return Ok(());
        }
        let mut write_one_qual = |flag, s| -> SerializeResult<()> {
            if sc.contains(flag) {
                self.w.write(s)?;
                self.write_space()?;
            }

            Ok(())
        };

        write_one_qual(StorageClass::CONST, b"const")?;
        // __restrict is different than `restrict`, keep the underscores!
        write_one_qual(StorageClass::RESTRICT, b"__restrict")?;
        // TODO: undname prints ref-qualifiers tightly to previous qualifiers.
        write_one_qual(StorageClass::LVALUE_QUAL, b"&")?;
        write_one_qual(StorageClass::RVALUE_QUAL, b"&&")?;

        Ok(())
    }

    // Write the "second half" of a given type.
    fn write_post(&mut self, t: &Type) -> SerializeResult<()> {
        match t {
            &Type::MemberFunction(_, _, ref params, sc, ref return_type)
            | &Type::NonMemberFunction(_, ref params, sc, ref return_type) => {
                write!(self.w, "(")?;
                self.write_types(&params.types)?;
                write!(self.w, ")")?;

                self.write_post(return_type)?;

                self.write_memfn_qualifiers(sc)?;
            }
            &Type::MemberFunctionPointer(_, _, _, ref params, sc, ref return_type) => {
                write!(self.w, "(")?;
                self.write_types(&params.types)?;
                write!(self.w, ")")?;

                self.write_post(return_type)?;

                if sc.contains(StorageClass::CONST) {
                    write!(self.w, "const")?;
                    self.write_space()?;
                }
            }
            &Type::CXXVBTable(ref names, _sc) => {
                self.write_scope(names)?;
                write!(self.w, "{}", "\'}")?; // the rest of the "operator"
            }
            &Type::Ptr(ref inner, _sc) | &Type::Ref(ref inner, _sc) => {
                match inner.as_ref() {
                    &Type::MemberFunction(_, _, _, _, _)
                    | &Type::NonMemberFunction(_, _, _, _)
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
            &Type::CXXVFTable(ref names, _) => {
                if !names.names.is_empty() {
                    write!(self.w, "{{for ")?;
                    for name in &names.names {
                        write!(self.w, "`")?;
                        self.write_one_name(name)?;
                        write!(self.w, "'")?;
                    }
                    self.w.write(b"}")?;
                }
            }
            &Type::VCallThunk(offset, _) => {
                write!(self.w, "{{{},{{flat}}}}' }}", offset)?;
            }
            _ => {}
        }
        Ok(())
    }

    // Write a function or template parameter list.
    fn write_types(&mut self, types: &[Type]) -> SerializeResult<()> {
        for param in types.iter().take(types.len() - 1) {
            self.write_pre(param)?;
            self.write_post(param)?;
            write!(self.w, ",")?;
        }
        if let Some(param) = types.last() {
            self.write_pre(param)?;
            self.write_post(param)?;
        }
        Ok(())
    }

    fn write_class(&mut self, names: &Symbol, s: &str) -> SerializeResult<()> {
        write!(self.w, "{}", s)?;
        write!(self.w, " ")?;
        self.write_name(names)?;
        Ok(())
    }

    fn write_space_pre(&mut self) -> SerializeResult<()> {
        if let Some(&c) = self.w.last() {
            if char::from(c).is_ascii_alphabetic() || c == b'&' || c == b'>' {
                write!(self.w, " ")?;
            }
        }
        Ok(())
    }
    fn write_space(&mut self) -> SerializeResult<()> {
        if let Some(&c) = self.w.last() {
            if char::from(c).is_ascii_alphabetic() || c == b'*' || c == b'&' || c == b'>' {
                write!(self.w, " ")?;
            }
        }
        Ok(())
    }

    fn write_operator_name(&mut self, op: &Operator) -> SerializeResult<()> {
        let s = match op {
            &Operator::Ctor => "ctor",
            &Operator::Dtor => "dtor",
            &Operator::New => "operator new",
            &Operator::Delete => "operator delete",
            &Operator::Equal => "operator=",
            &Operator::RShift => "operator>>",
            &Operator::LShift => "operator<<",
            &Operator::Bang => "operator!",
            &Operator::EqualEqual => "operator==",
            &Operator::BangEqual => "operator!=",
            &Operator::Subscript => "operator[]",
            &Operator::Conversion => "operatorcast",
            &Operator::Arrow => "operator->",
            &Operator::Star => "operator*",
            &Operator::PlusPlus => "operator++",
            &Operator::MinusMinus => "operator--",
            &Operator::Minus => "operator-",
            &Operator::Plus => "operator+",
            &Operator::Amp => "operator&",
            &Operator::ArrowStar => "operator->*",
            &Operator::Slash => "operator/",
            &Operator::Percent => "operator%",
            &Operator::Less => "operator<",
            &Operator::LessEqual => "operator<=",
            &Operator::Greater => "operator>",
            &Operator::GreaterEqual => "operator>=",
            &Operator::Comma => "operator,",
            &Operator::Call => "operator()",
            &Operator::Tilde => "operator~",
            &Operator::Caret => "operator^",
            &Operator::Pipe => "operator|",
            &Operator::AmpAmp => "operator&&",
            &Operator::PipePipe => "operator||",
            &Operator::StarEqual => "operator*=",
            &Operator::PlusEqual => "operator+=",
            &Operator::MinusEqual => "operator-=",
            &Operator::SlashEqual => "operator/=",
            &Operator::PercentEqual => "operator%=",
            &Operator::GreaterGreaterEqual => "operator>>=",
            &Operator::LessLessEqual => "operator<<=",
            &Operator::AmpEqual => "operator&=",
            &Operator::PipeEqual => "operator|=",
            &Operator::CaretEqual => "operator^=",

            &Operator::VFTable => "`vftable'",
            &Operator::VBTable => "`vbtable'",
            &Operator::VCall => "`vcall'",
            &Operator::Typeof => "`typeof'",
            &Operator::LocalStaticGuard => "`local static guard'",
            &Operator::String => "`string'",
            &Operator::VBaseDtor => "`vbase destructor'",
            &Operator::VectorDeletingDtor => "`vector deleting destructor'",
            &Operator::DefaultCtorClosure => "`default constructor closure'",
            &Operator::ScalarDeletingDtor => "`scalar deleting destructor'",
            &Operator::VectorCtorIterator => "`vector constructor iterator'",
            &Operator::VectorDtorIterator => "`vector destructor iterator'",
            &Operator::VectorVBaseCtorIterator => "`vector vbase constructor iterator'",
            &Operator::VirtualDisplacementMap => "`virual displacement map'",
            &Operator::EHVectorCtorIterator => "`eh vector constructor iterator'",
            &Operator::EHVectorDtorIterator => "`eh vector destructor iterator'",
            &Operator::EHVectorVBaseCtorIterator => "`eh vector vbase constructor iterator'",
            &Operator::CopyCtorClosure => "`copy constructor closure",

            &Operator::LocalVFTable => "`local vftable'",
            &Operator::LocalVFTableCtorClosure => "`local vftable constructor closure'",
            &Operator::ArrayNew => "operator new[]",
            &Operator::ArrayDelete => "operator delete[]",
            &Operator::PlacementDeleteClosure => "`placement delete closure'",
            &Operator::PlacementArrayDeleteClosure => "`placement delete[] closure'",

            &Operator::CoroutineAwait => " co_await",
            &Operator::LiteralOperatorName => " CXXLiteralOperatorName",

            &Operator::RTTITypeDescriptor(_, ref inner) => {
                self.write_pre(inner)?;
                write!(self.w, "::`RTTI Type Descriptor'")?;
                return Ok(());
            }
            &Operator::RTTIBaseClassDescriptor(nv_offset, vbptr_offset, vbtable_offset, flags) => {
                write!(
                    self.w,
                    "`RTTI Base Class Descriptor at ({},{},{},{})'",
                    nv_offset, vbptr_offset, vbtable_offset, flags
                )?;
                return Ok(());
            }
            &Operator::RTTIBaseClassArray => "`RTTI Base Class Array'",
            &Operator::RTTIClassHierarchyDescriptor => "`RTTI Class Hierarchy Descriptor'",
        };
        write!(self.w, "{}", s)?;
        Ok(())
    }

    fn write_one_name(&mut self, name: &Name) -> SerializeResult<()> {
        match name {
            &Name::Operator(ref op) => {
                self.write_space()?;
                self.write_operator_name(op)?;
                //panic!("only the last name should be an operator");
            }
            &Name::NonTemplate(ref name) => {
                self.w.write(name)?;
            }
            &Name::Template(ref name, ref params) => {
                self.write_one_name(name)?;
                self.write_tmpl_params(&params)?;
            }
            &Name::Discriminator(ref val) => {
                write!(self.w, "`{}'", val)?;
            }
            &Name::ParsedName(ref val) => {
                write!(self.w, "`{}'", serialize(val, self.flags).unwrap())?;
            }
            &Name::AnonymousNamespace => {
                write!(self.w, "`anonymous namespace`")?;
            }
        }
        Ok(())
    }

    fn write_scope(&mut self, names: &NameSequence) -> SerializeResult<()> {
        // Print out namespaces or outer class names.
        let mut i = names.names.iter().rev();
        if let Some(name) = i.next() {
            self.write_one_name(&name)?;
        }
        for name in i {
            write!(self.w, "::")?;
            self.write_one_name(&name)?;
        }
        Ok(())
    }

    // Write a name read by read_name().
    fn write_name(&mut self, names: &Symbol) -> SerializeResult<()> {
        self.write_space_pre()?;

        self.write_scope(&names.scope)?;

        if !names.scope.names.is_empty() {
            write!(self.w, "::")?;
        }

        match &names.name {
            &Name::Operator(ref op) => {
                match op {
                    &Operator::Ctor => {
                        let prev = names.scope.names.iter().nth(0).expect(
                            "If there's a ctor, there should be another name in this sequence",
                        );
                        self.write_one_name(prev)?;
                    }
                    &Operator::Dtor => {
                        let prev = names.scope.names.iter().nth(0).expect(
                            "If there's a dtor, there should be another name in this sequence",
                        );
                        write!(self.w, "~")?;
                        self.write_one_name(prev)?;
                    }
                    &Operator::VBTable => {
                        write!(self.w, "{}", "`vbtable'{for `")?;
                        // The rest will be written by write_post of the
                        // symbol type.
                    }
                    _ => {
                        self.write_space()?;
                        // Print out an overloaded operator.
                        self.write_operator_name(op)?;
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
            &Name::Discriminator(ref val) => {
                write!(self.w, "`{}'", val)?;
            }
            &Name::ParsedName(ref val) => {
                write!(self.w, "{}", serialize(val, self.flags).unwrap())?;
            }
            &Name::AnonymousNamespace => {
                panic!("not supposed to be here");
            }
        }
        Ok(())
    }

    fn write_tmpl_params<'b>(&mut self, params: &Params<'b>) -> SerializeResult<()> {
        let types = if let Some(&Type::EmptyParameterPack) = params.types.last() {
            &params.types[0..params.types.len() - 1]
        } else {
            &params.types
        };

        write!(self.w, "<")?;
        if !types.is_empty() {
            self.write_types(types)?;
            if let Some(&b'>') = self.w.last() {
                write!(self.w, " ")?;
            }
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
    fn expect_with_flags(input: &str, reference: &str, flags: u32) {
        let demangled: ::Result<_> = ::demangle(input, ::DemangleFlags::from_bits(flags).unwrap());
        let reference: ::Result<_> = Ok(reference.to_owned());
        assert_eq!(demangled, reference);
    }

    // For cases where undname demangles differently/better than we do.
    fn expect_failure(input: &str, reference: &str) {
        let demangled: ::Result<_> = ::demangle(input, ::DemangleFlags::COMPLETE);
        let reference: ::Result<_> = Ok(reference.to_owned());
        assert_ne!(demangled, reference);
    }
    // std::basic_filebuf<char,struct std::char_traits<char> >::basic_filebuf<char,struct std::char_traits<char> >
    // std::basic_filebuf<char,struct std::char_traits<char> >::"operator ctor"
    // "operator ctor" = ?0

    #[test]
    fn other_tests() {
        let expect = |input, reference| {
            expect_with_flags(input, reference, 0x0);
        };

        expect("?f@@YAHQBH@Z", "int __cdecl f(int const * const)");
        expect("?f@@YA_WQB_W@Z", "wchar_t __cdecl f(wchar_t const * const)");
        expect(
            "?f@@YA_UQB_U@Z",
            "char32_t __cdecl f(char32_t const * const)",
        );
        expect(
            "?f@@YA_SQB_S@Z",
            "char16_t __cdecl f(char16_t const * const)",
        );
        expect(
            "?g@@YAHQAY0EA@$$CBH@Z",
            "int __cdecl g(int const (* const)[64])",
        );
        expect(
            "??0Klass@std@@AEAA@AEBV01@@Z",
            "private: __cdecl std::Klass::Klass(class std::Klass const &)",
        );
        expect("??0?$Klass@V?$Mass@_N@@@std@@QEAA@AEBV01@@Z",
               "public: __cdecl std::Klass<class Mass<bool> >::Klass<class Mass<bool> >(class std::Klass<class Mass<bool> > const &)");
        expect(
            "??$load@M@UnsharedOps@js@@SAMV?$SharedMem@PAM@@@Z",
            "public: static float __cdecl js::UnsharedOps::load<float>(class SharedMem<float *>)",
        );

        expect("?cached@?1??GetLong@BinaryPath@mozilla@@SA?AW4nsresult@@QA_W@Z@4_NA",
               "bool `public: static enum nsresult __cdecl mozilla::BinaryPath::GetLong(wchar_t * const)\'::`2\'::cached");
        expect(
            "??0?$A@_K@B@@QAE@$$QAV01@@Z",
            "public: __thiscall B::A<uint64_t>::A<uint64_t>(class B::A<uint64_t> &&)",
        );
        expect("??_7nsI@@6B@", "const nsI::`vftable\'");
        expect("??_7W@?A@@6B@", "const `anonymous namespace`::W::`vftable'");
        expect(
            "??_7?$RunnableMethodImpl@PEAVLazyIdleThread@mozilla@@P812@EAAXXZ$0A@$0A@$$V@detail@mozilla@@6BnsIRunnable@@@",
            "const mozilla::detail::RunnableMethodImpl<class mozilla::LazyIdleThread *,void __cdecl (mozilla::LazyIdleThread::*)(void),0,0>::`vftable\'{for `nsIRunnable\'}",
        );
        expect_failure(
            "??_7?$RunnableMethodImpl@PEAVLazyIdleThread@mozilla@@P812@EAAXXZ$0A@$0A@$$V@detail@mozilla@@6BnsIRunnable@@@",
            "const mozilla::detail::RunnableMethodImpl<class mozilla::LazyIdleThread * __ptr64,void __cdecl (mozilla::LazyIdleThread::*)(void) __ptr64,0,0>::`vftable\'{for `nsIRunnable\'}",
        );
        expect(
            "??1?$ns@$$CBVtxXP@@@@QAE@XZ",
            "public: __thiscall ns<class txXP const>::~ns<class txXP const>(void)",
        );
        /* XXX: undname prints void (__thiscall*)(void *) for the parameter type. */
        expect(
            "??_I@YGXPAXIIP6EX0@Z@Z",
            "void __stdcall `vector destructor iterator'(void *,unsigned int,unsigned int,void __thiscall (*)(void *))",
        );
        expect(
            "??_GnsWindowsShellService@@EAEPAXI@Z",
            "private: virtual void * __thiscall nsWindowsShellService::`scalar deleting destructor'(unsigned int)",
        );
        expect(
            "??1?$nsAutoPtr@$$CBVtxXPathNode@@@@QAE@XZ",
            "public: __thiscall nsAutoPtr<class txXPathNode const>::~nsAutoPtr<class txXPathNode const>(void)",
        );
        expect(
            "??_EPrintfTarget@mozilla@@MAEPAXI@Z",
            "protected: virtual void * __thiscall mozilla::PrintfTarget::`vector deleting destructor'(unsigned int)",
        );
        expect(
            "??_GDynamicFrameEventFilter@?A0xcdaa5fa8@@AAEPAXI@Z",
            "private: void * __thiscall `anonymous namespace`::DynamicFrameEventFilter::`scalar deleting destructor\'(unsigned int)",
        );
        /* XXX: undname tacks on `adjustor{16}` to the name. */
        expect(
            "?Release@ContentSignatureVerifier@@WBA@AGKXZ",
            "[thunk]:public: virtual unsigned long __stdcall ContentSignatureVerifier::Release(void)",
        );
        expect(
            "??$new_@VWatchpointMap@js@@$$V@?$MallocProvider@UZone@JS@@@js@@QAEPAVWatchpointMap@1@XZ",
            "public: class js::WatchpointMap * __thiscall js::MallocProvider<struct JS::Zone>::new_<class js::WatchpointMap>(void)",
        );
        expect(
            "??$templ_fun_with_ty_pack@$$V@@YAXXZ",
            "void __cdecl templ_fun_with_ty_pack<>(void)",
        );
        expect(
            "??4?$RefPtr@VnsRange@@@@QAEAAV0@$$T@Z",
            "public: class RefPtr<class nsRange> & __thiscall RefPtr<class nsRange>::operator=(std::nullptr_t)",
        );
        expect(
            "??1?$function@$$A6AXXZ@std@@QAE@XZ",
            "public: __thiscall std::function<void __cdecl (void)>::~function<void __cdecl (void)>(void)",
        );
        expect_failure(
            "??1?$function@$$A6AXXZ@std@@QAE@XZ",
            "public: __thiscall std::function<void __cdecl(void)>::~function<void __cdecl(void)>(void)",
        );
        // Not great (`operatorcast`, space at the end), but at least make sure we don't regress.
        expect(
            "??B?$function@$$A6AXXZ@std@@QBE_NXZ",
            "public: bool __thiscall std::function<void __cdecl (void)>::operatorcast(void)const ",
        );
        expect_failure(
            "??B?$function@$$A6AXXZ@std@@QBE_NXZ",
            "public: __thiscall std::function<void __cdecl(void)>::operator bool(void)const",
        );
        expect(
            "??$?RA6AXXZ$$V@SkOnce@@QAEXA6AXXZ@Z",
            "public: void __thiscall SkOnce::operator()<void __cdecl (&)(void)>(void __cdecl (&)(void))",
        );
        expect_failure(
            "??$?RA6AXXZ$$V@SkOnce@@QAEXA6AXXZ@Z",
            "public: void __thiscall SkOnce::operator()<void (__cdecl&)(void)>(void (__cdecl&)(void))",
        );
        expect(
            "?foo@A@PR19361@@QIHAEXXZ",
            "public: void __thiscall PR19361::A::foo(void)__restrict && ",
        );
        expect_failure(
            "?foo@A@PR19361@@QIHAEXXZ",
            "public: void __thiscall PR19361::A::foo(void) __restrict&& ",
        );
        expect(
            "??$GenericCreateConstructor@$1?construct@SetObject@js@@CA_NPEAUJSContext@@IPEATValue@JS@@@Z$0A@$0A@$0A@@js@@YAPEAVJSObject@@PEAUJSContext@@W4JSProtoKey@@@Z",
            "class JSObject * __cdecl js::GenericCreateConstructor<bool __cdecl (js::SetObject::construct::*)(struct JSContext *,unsigned int,union JS::Value *),0,0,0>(struct JSContext *,enum JSProtoKey)",
        );
        expect_failure(
            "??$GenericCreateConstructor@$1?construct@SetObject@js@@CA_NPEAUJSContext@@IPEATValue@JS@@@Z$0A@$0A@$0A@@js@@YAPEAVJSObject@@PEAUJSContext@@W4JSProtoKey@@@Z",
            "class JSObject * __ptr64 __cdecl js::GenericCreateConstructor<&private: static bool __cdecl (js::SetObject::construct::*)(struct JSContext * __ptr64,unsigned int,union JS::Value * __ptr64),0,0,0>(struct JSContext * __ptr64,enum JSProtoKey)",
        );
        expect(
            "??$emplace_hint@AEBUpiecewise_construct_t@std@@V?$tuple@AEBH@2@V?$tuple@$$V@2@@?$_Tree@V?$_Tmap_traits@HUPayload@RtpUtility@webrtc@@U?$less@H@std@@V?$allocator@U?$pair@$$CBHUPayload@RtpUtility@webrtc@@@std@@@5@$0A@@std@@@std@@QEAA?AV?$_Tree_iterator@V?$_Tree_val@U?$_Tree_simple_types@U?$pair@$$CBHUPayload@RtpUtility@webrtc@@@std@@@std@@@std@@@1@V?$_Tree_const_iterator@V?$_Tree_val@U?$_Tree_simple_types@U?$pair@$$CBHUPayload@RtpUtility@webrtc@@@std@@@std@@@std@@@1@AEBUpiecewise_construct_t@1@$$QEAV?$tuple@AEBH@1@$$QEAV?$tuple@$$V@1@@Z",
            "public: class std::_Tree_iterator<class std::_Tree_val<struct std::_Tree_simple_types<struct std::pair<int const,struct webrtc::RtpUtility::Payload> > > > __cdecl std::_Tree<class std::_Tmap_traits<int,struct webrtc::RtpUtility::Payload,struct std::less<int>,class std::allocator<struct std::pair<int const,struct webrtc::RtpUtility::Payload> >,0> >::emplace_hint<struct std::piecewise_construct_t const &,class std::tuple<int const &>,class std::tuple<> >(class std::_Tree_const_iterator<class std::_Tree_val<struct std::_Tree_simple_types<struct std::pair<int const,struct webrtc::RtpUtility::Payload> > > >,struct std::piecewise_construct_t const &,class std::tuple<int const &> &&,class std::tuple<> &&)",
        );
        expect(
            "?_OptionsStorage@?1??__local_stdio_scanf_options@@9@9",
            "`__local_stdio_scanf_options'::`2'::_OptionsStorage",
        );
        expect(
            "??_9nsDocument@@$BDMI@AE",
            "[thunk]: __thiscall nsDocument::`vcall'{968,{flat}}' }",
        );
        expect(
            "??_R0?AUCollationCacheEntry@icu_61@@@8",
            "struct icu_61::CollationCacheEntry::`RTTI Type Descriptor\'",
        );
        expect(
            "??_R0?AV?$KxTree@V?$KxSpe@DI@@I@@@8",
            "class KxTree<class KxSpe<char,unsigned int>,unsigned int>::`RTTI Type Descriptor'",
        );
        expect("??_R2A@@8", "A::`RTTI Base Class Array'");
        expect("??_R3UO@i@@8", "i::UO::`RTTI Class Hierarchy Descriptor'");
        expect(
            "??_R1A@?0A@EA@U@i@@8",
            "i::U::`RTTI Base Class Descriptor at (0,-1,0,64)'",
        )
    }

    #[test]
    fn test_strings() {
        let expect = |input, reference| {
            expect_with_flags(input, reference, 0x0);
        };

        // Test symbols extracted from clang's test/CodeGenCXX/mangle-ms-string-literals.cpp.
        // Even though we don't print the encoded strings, these tests
        // exhaustively cover all the cases we'll run into.

        // Single-byte characters.
        expect("??_C@_01CNACBAHC@?$PP?$AA@", "`string'");
        expect("??_C@_01DEBJCBDD@?$PO?$AA@", "`string'");
        expect("??_C@_01BPDEHCPA@?$PN?$AA@", "`string'");
        expect("??_C@_01GCPEDLB@?$PM?$AA@", "`string'");
        expect("??_C@_01EJGONFHG@?$PL?$AA@", "`string'");
        expect("??_C@_01FAHFOEDH@?z?$AA@", "`string'");
        expect("??_C@_01HLFILHPE@?y?$AA@", "`string'");
        expect("??_C@_01GCEDIGLF@?x?$AA@", "`string'");
        expect("??_C@_01OFNLJKHK@?w?$AA@", "`string'");
        expect("??_C@_01PMMAKLDL@?v?$AA@", "`string'");
        expect("??_C@_01NHONPIPI@?u?$AA@", "`string'");
        expect("??_C@_01MOPGMJLJ@?t?$AA@", "`string'");
        expect("??_C@_01IBLHFPHO@?s?$AA@", "`string'");
        expect("??_C@_01JIKMGODP@?r?$AA@", "`string'");
        expect("??_C@_01LDIBDNPM@?q?$AA@", "`string'");
        expect("??_C@_01KKJKAMLN@?p?$AA@", "`string'");
        expect("??_C@_01GHMAACCD@?o?$AA@", "`string'");
        expect("??_C@_01HONLDDGC@?n?$AA@", "`string'");
        expect("??_C@_01FFPGGAKB@?m?$AA@", "`string'");
        expect("??_C@_01EMONFBOA@?l?$AA@", "`string'");
        expect("??_C@_01DKMMHCH@?k?$AA@", "`string'");
        expect("??_C@_01BKLHPGGG@?j?$AA@", "`string'");
        expect("??_C@_01DBJKKFKF@?i?$AA@", "`string'");
        expect("??_C@_01CIIBJEOE@?h?$AA@", "`string'");
        expect("??_C@_01KPBJIICL@?g?$AA@", "`string'");
        expect("??_C@_01LGACLJGK@?f?$AA@", "`string'");
        expect("??_C@_01JNCPOKKJ@?e?$AA@", "`string'");
        expect("??_C@_01IEDENLOI@?d?$AA@", "`string'");
        expect("??_C@_01MLHFENCP@?c?$AA@", "`string'");
        expect("??_C@_01NCGOHMGO@?b?$AA@", "`string'");
        expect("??_C@_01PJEDCPKN@?a?$AA@", "`string'");
        expect("??_C@_01OAFIBOOM@?$OA?$AA@", "`string'");
        expect("??_C@_01LIIGDENA@?$NP?$AA@", "`string'");
        expect("??_C@_01KBJNAFJB@?$NO?$AA@", "`string'");
        expect("??_C@_01IKLAFGFC@?$NN?$AA@", "`string'");
        expect("??_C@_01JDKLGHBD@?$NM?$AA@", "`string'");
        expect("??_C@_01NMOKPBNE@?$NL?$AA@", "`string'");
        expect("??_C@_01MFPBMAJF@?Z?$AA@", "`string'");
        expect("??_C@_01OONMJDFG@?Y?$AA@", "`string'");
        expect("??_C@_01PHMHKCBH@?X?$AA@", "`string'");
        expect("??_C@_01HAFPLONI@?W?$AA@", "`string'");
        expect("??_C@_01GJEEIPJJ@?V?$AA@", "`string'");
        expect("??_C@_01ECGJNMFK@?U?$AA@", "`string'");
        expect("??_C@_01FLHCONBL@?T?$AA@", "`string'");
        expect("??_C@_01BEDDHLNM@?S?$AA@", "`string'");
        expect("??_C@_01NCIEKJN@?R?$AA@", "`string'");
        expect("??_C@_01CGAFBJFO@?Q?$AA@", "`string'");
        expect("??_C@_01DPBOCIBP@?P?$AA@", "`string'");
        expect("??_C@_01PCEECGIB@?O?$AA@", "`string'");
        expect("??_C@_01OLFPBHMA@?N?$AA@", "`string'");
        expect("??_C@_01MAHCEEAD@?M?$AA@", "`string'");
        expect("??_C@_01NJGJHFEC@?L?$AA@", "`string'");
        expect("??_C@_01JGCIODIF@?K?$AA@", "`string'");
        expect("??_C@_01IPDDNCME@?J?$AA@", "`string'");
        expect("??_C@_01KEBOIBAH@?I?$AA@", "`string'");
        expect("??_C@_01LNAFLAEG@?H?$AA@", "`string'");
        expect("??_C@_01DKJNKMIJ@?G?$AA@", "`string'");
        expect("??_C@_01CDIGJNMI@?F?$AA@", "`string'");
        expect("??_C@_01IKLMOAL@?E?$AA@", "`string'");
        expect("??_C@_01BBLAPPEK@?D?$AA@", "`string'");
        expect("??_C@_01FOPBGJIN@?C?$AA@", "`string'");
        expect("??_C@_01EHOKFIMM@?B?$AA@", "`string'");
        expect("??_C@_01GMMHALAP@?A?$AA@", "`string'");
        expect("??_C@_01HFNMDKEO@?$MA?$AA@", "`string'");
        expect("??_C@_01NNHLFPHH@?$LP?$AA@", "`string'");
        expect("??_C@_01MEGAGODG@?$LO?$AA@", "`string'");
        expect("??_C@_01OPENDNPF@?$LN?$AA@", "`string'");
        expect("??_C@_01PGFGAMLE@?$LM?$AA@", "`string'");
        expect("??_C@_01LJBHJKHD@?$LL?$AA@", "`string'");
        expect("??_C@_01KAAMKLDC@?$LK?$AA@", "`string'");
        expect("??_C@_01ILCBPIPB@?$LJ?$AA@", "`string'");
        expect("??_C@_01JCDKMJLA@?$LI?$AA@", "`string'");
        expect("??_C@_01BFKCNFHP@?$LH?$AA@", "`string'");
        expect("??_C@_01MLJOEDO@?$LG?$AA@", "`string'");
        expect("??_C@_01CHJELHPN@?$LF?$AA@", "`string'");
        expect("??_C@_01DOIPIGLM@?$LE?$AA@", "`string'");
        expect("??_C@_01HBMOBAHL@?$LD?$AA@", "`string'");
        expect("??_C@_01GINFCBDK@?$LC?$AA@", "`string'");
        expect("??_C@_01EDPIHCPJ@?$LB?$AA@", "`string'");
        expect("??_C@_01FKODEDLI@?$LA?$AA@", "`string'");
        expect("??_C@_01JHLJENCG@?$KP?$AA@", "`string'");
        expect("??_C@_01IOKCHMGH@?$KO?$AA@", "`string'");
        expect("??_C@_01KFIPCPKE@?$KN?$AA@", "`string'");
        expect("??_C@_01LMJEBOOF@?$KM?$AA@", "`string'");
        expect("??_C@_01PDNFIICC@?$KL?$AA@", "`string'");
        expect("??_C@_01OKMOLJGD@?$KK?$AA@", "`string'");
        expect("??_C@_01MBODOKKA@?$KJ?$AA@", "`string'");
        expect("??_C@_01NIPINLOB@?$KI?$AA@", "`string'");
        expect("??_C@_01FPGAMHCO@?$KH?$AA@", "`string'");
        expect("??_C@_01EGHLPGGP@?$KG?$AA@", "`string'");
        expect("??_C@_01GNFGKFKM@?$KF?$AA@", "`string'");
        expect("??_C@_01HEENJEON@?$KE?$AA@", "`string'");
        expect("??_C@_01DLAMACCK@?$KD?$AA@", "`string'");
        expect("??_C@_01CCBHDDGL@?$KC?$AA@", "`string'");
        expect("??_C@_01JDKGAKI@?$KB?$AA@", "`string'");
        expect("??_C@_01BACBFBOJ@?$KA?$AA@", "`string'");
        expect("??_C@_01EIPPHLNF@?$JP?$AA@", "`string'");
        expect("??_C@_01FBOEEKJE@?$JO?$AA@", "`string'");
        expect("??_C@_01HKMJBJFH@?$JN?$AA@", "`string'");
        expect("??_C@_01GDNCCIBG@?$JM?$AA@", "`string'");
        expect("??_C@_01CMJDLONB@?$JL?$AA@", "`string'");
        expect("??_C@_01DFIIIPJA@?$JK?$AA@", "`string'");
        expect("??_C@_01BOKFNMFD@?$JJ?$AA@", "`string'");
        expect("??_C@_01HLOONBC@?$JI?$AA@", "`string'");
        expect("??_C@_01IACGPBNN@?$JH?$AA@", "`string'");
        expect("??_C@_01JJDNMAJM@?$JG?$AA@", "`string'");
        expect("??_C@_01LCBAJDFP@?$JF?$AA@", "`string'");
        expect("??_C@_01KLALKCBO@?$JE?$AA@", "`string'");
        expect("??_C@_01OEEKDENJ@?$JD?$AA@", "`string'");
        expect("??_C@_01PNFBAFJI@?$JC?$AA@", "`string'");
        expect("??_C@_01NGHMFGFL@?$JB?$AA@", "`string'");
        expect("??_C@_01MPGHGHBK@?$JA?$AA@", "`string'");
        expect("??_C@_01CDNGJIE@?$IP?$AA@", "`string'");
        expect("??_C@_01BLCGFIMF@?$IO?$AA@", "`string'");
        expect("??_C@_01DAALALAG@?$IN?$AA@", "`string'");
        expect("??_C@_01CJBADKEH@?$IM?$AA@", "`string'");
        expect("??_C@_01GGFBKMIA@?$IL?$AA@", "`string'");
        expect("??_C@_01HPEKJNMB@?$IK?$AA@", "`string'");
        expect("??_C@_01FEGHMOAC@?$IJ?$AA@", "`string'");
        expect("??_C@_01ENHMPPED@?$II?$AA@", "`string'");
        expect("??_C@_01MKOEODIM@?$IH?$AA@", "`string'");
        expect("??_C@_01NDPPNCMN@?$IG?$AA@", "`string'");
        expect("??_C@_01PINCIBAO@?$IF?$AA@", "`string'");
        expect("??_C@_01OBMJLAEP@?$IE?$AA@", "`string'");
        expect("??_C@_01KOIICGII@?$ID?$AA@", "`string'");
        expect("??_C@_01LHJDBHMJ@?$IC?$AA@", "`string'");
        expect("??_C@_01JMLOEEAK@?$IB?$AA@", "`string'");
        expect("??_C@_01IFKFHFEL@?$IA?$AA@", "`string'");
        expect("??_C@_01BGIBIIDJ@?$HP?$AA@", "`string'");
        expect("??_C@_01PJKLJHI@?$HO?$AA@", "`string'");
        expect("??_C@_01CELHOKLL@?$HN?$AA@", "`string'");
        expect("??_C@_01DNKMNLPK@?$HM?$AA@", "`string'");
        expect("??_C@_01HCONENDN@?$HL?$AA@", "`string'");
        expect("??_C@_01GLPGHMHM@z?$AA@", "`string'");
        expect("??_C@_01EANLCPLP@y?$AA@", "`string'");
        expect("??_C@_01FJMABOPO@x?$AA@", "`string'");
        expect("??_C@_01NOFIACDB@w?$AA@", "`string'");
        expect("??_C@_01MHEDDDHA@v?$AA@", "`string'");
        expect("??_C@_01OMGOGALD@u?$AA@", "`string'");
        expect("??_C@_01PFHFFBPC@t?$AA@", "`string'");
        expect("??_C@_01LKDEMHDF@s?$AA@", "`string'");
        expect("??_C@_01KDCPPGHE@r?$AA@", "`string'");
        expect("??_C@_01IIACKFLH@q?$AA@", "`string'");
        expect("??_C@_01JBBJJEPG@p?$AA@", "`string'");
        expect("??_C@_01FMEDJKGI@o?$AA@", "`string'");
        expect("??_C@_01EFFIKLCJ@n?$AA@", "`string'");
        expect("??_C@_01GOHFPIOK@m?$AA@", "`string'");
        expect("??_C@_01HHGOMJKL@l?$AA@", "`string'");
        expect("??_C@_01DICPFPGM@k?$AA@", "`string'");
        expect("??_C@_01CBDEGOCN@j?$AA@", "`string'");
        expect("??_C@_01KBJDNOO@i?$AA@", "`string'");
        expect("??_C@_01BDACAMKP@h?$AA@", "`string'");
        expect("??_C@_01JEJKBAGA@g?$AA@", "`string'");
        expect("??_C@_01INIBCBCB@f?$AA@", "`string'");
        expect("??_C@_01KGKMHCOC@e?$AA@", "`string'");
        expect("??_C@_01LPLHEDKD@d?$AA@", "`string'");
        expect("??_C@_01PAPGNFGE@c?$AA@", "`string'");
        expect("??_C@_01OJONOECF@b?$AA@", "`string'");
        expect("??_C@_01MCMALHOG@a?$AA@", "`string'");
        expect("??_C@_01NLNLIGKH@?$GA?$AA@", "`string'");
        expect("??_C@_01IDAFKMJL@_?$AA@", "`string'");
        expect("??_C@_01JKBOJNNK@?$FO?$AA@", "`string'");
        expect("??_C@_01LBDDMOBJ@?$FN?$AA@", "`string'");
        expect("??_C@_01KICIPPFI@?2?$AA@", "`string'");
        expect("??_C@_01OHGJGJJP@?$FL?$AA@", "`string'");
        expect("??_C@_01POHCFINO@Z?$AA@", "`string'");
        expect("??_C@_01NFFPALBN@Y?$AA@", "`string'");
        expect("??_C@_01MMEEDKFM@X?$AA@", "`string'");
        expect("??_C@_01ELNMCGJD@W?$AA@", "`string'");
        expect("??_C@_01FCMHBHNC@V?$AA@", "`string'");
        expect("??_C@_01HJOKEEBB@U?$AA@", "`string'");
        expect("??_C@_01GAPBHFFA@T?$AA@", "`string'");
        expect("??_C@_01CPLAODJH@S?$AA@", "`string'");
        expect("??_C@_01DGKLNCNG@R?$AA@", "`string'");
        expect("??_C@_01BNIGIBBF@Q?$AA@", "`string'");
        expect("??_C@_01EJNLAFE@P?$AA@", "`string'");
        expect("??_C@_01MJMHLOMK@O?$AA@", "`string'");
        expect("??_C@_01NANMIPIL@N?$AA@", "`string'");
        expect("??_C@_01PLPBNMEI@M?$AA@", "`string'");
        expect("??_C@_01OCOKONAJ@L?$AA@", "`string'");
        expect("??_C@_01KNKLHLMO@K?$AA@", "`string'");
        expect("??_C@_01LELAEKIP@J?$AA@", "`string'");
        expect("??_C@_01JPJNBJEM@I?$AA@", "`string'");
        expect("??_C@_01IGIGCIAN@H?$AA@", "`string'");
        expect("??_C@_01BBODEMC@G?$AA@", "`string'");
        expect("??_C@_01BIAFAFID@F?$AA@", "`string'");
        expect("??_C@_01DDCIFGEA@E?$AA@", "`string'");
        expect("??_C@_01CKDDGHAB@D?$AA@", "`string'");
        expect("??_C@_01GFHCPBMG@C?$AA@", "`string'");
        expect("??_C@_01HMGJMAIH@B?$AA@", "`string'");
        expect("??_C@_01FHEEJDEE@A?$AA@", "`string'");
        expect("??_C@_01EOFPKCAF@?$EA?$AA@", "`string'");
        expect("??_C@_01OGPIMHDM@?$DP?$AA@", "`string'");
        expect("??_C@_01PPODPGHN@?$DO?$AA@", "`string'");
        expect("??_C@_01NEMOKFLO@?$DN?$AA@", "`string'");
        expect("??_C@_01MNNFJEPP@?$DM?$AA@", "`string'");
        expect("??_C@_01ICJEACDI@?$DL?$AA@", "`string'");
        expect("??_C@_01JLIPDDHJ@?3?$AA@", "`string'");
        expect("??_C@_01LAKCGALK@9?$AA@", "`string'");
        expect("??_C@_01KJLJFBPL@8?$AA@", "`string'");
        expect("??_C@_01COCBENDE@7?$AA@", "`string'");
        expect("??_C@_01DHDKHMHF@6?$AA@", "`string'");
        expect("??_C@_01BMBHCPLG@5?$AA@", "`string'");
        expect("??_C@_01FAMBOPH@4?$AA@", "`string'");
        expect("??_C@_01EKENIIDA@3?$AA@", "`string'");
        expect("??_C@_01FDFGLJHB@2?$AA@", "`string'");
        expect("??_C@_01HIHLOKLC@1?$AA@", "`string'");
        expect("??_C@_01GBGANLPD@0?$AA@", "`string'");
        expect("??_C@_01KMDKNFGN@?1?$AA@", "`string'");
        expect("??_C@_01LFCBOECM@?4?$AA@", "`string'");
        expect("??_C@_01JOAMLHOP@?9?$AA@", "`string'");
        expect("??_C@_01IHBHIGKO@?0?$AA@", "`string'");
        expect("??_C@_01MIFGBAGJ@?$CL?$AA@", "`string'");
        expect("??_C@_01NBENCBCI@?$CK?$AA@", "`string'");
        expect("??_C@_01PKGAHCOL@?$CJ?$AA@", "`string'");
        expect("??_C@_01ODHLEDKK@?$CI?$AA@", "`string'");
        expect("??_C@_01GEODFPGF@?8?$AA@", "`string'");
        expect("??_C@_01HNPIGOCE@?$CG?$AA@", "`string'");
        expect("??_C@_01FGNFDNOH@?$CF?$AA@", "`string'");
        expect("??_C@_01EPMOAMKG@$?$AA@", "`string'");
        expect("??_C@_01IPJKGB@?$CD?$AA@", "`string'");
        expect("??_C@_01BJJEKLCA@?$CC?$AA@", "`string'");
        expect("??_C@_01DCLJPIOD@?$CB?$AA@", "`string'");
        expect("??_C@_01CLKCMJKC@?5?$AA@", "`string'");
        expect("??_C@_01HDHMODJO@?$BP?$AA@", "`string'");
        expect("??_C@_01GKGHNCNP@?$BO?$AA@", "`string'");
        expect("??_C@_01EBEKIBBM@?$BN?$AA@", "`string'");
        expect("??_C@_01FIFBLAFN@?$BM?$AA@", "`string'");
        expect("??_C@_01BHBACGJK@?$BL?$AA@", "`string'");
        expect("??_C@_01OALBHNL@?$BK?$AA@", "`string'");
        expect("??_C@_01CFCGEEBI@?$BJ?$AA@", "`string'");
        expect("??_C@_01DMDNHFFJ@?$BI?$AA@", "`string'");
        expect("??_C@_01LLKFGJJG@?$BH?$AA@", "`string'");
        expect("??_C@_01KCLOFINH@?$BG?$AA@", "`string'");
        expect("??_C@_01IJJDALBE@?$BF?$AA@", "`string'");
        expect("??_C@_01JAIIDKFF@?$BE?$AA@", "`string'");
        expect("??_C@_01NPMJKMJC@?$BD?$AA@", "`string'");
        expect("??_C@_01MGNCJNND@?$BC?$AA@", "`string'");
        expect("??_C@_01ONPPMOBA@?$BB?$AA@", "`string'");
        expect("??_C@_01PEOEPPFB@?$BA?$AA@", "`string'");
        expect("??_C@_01DJLOPBMP@?$AP?$AA@", "`string'");
        expect("??_C@_01CAKFMAIO@?$AO?$AA@", "`string'");
        expect("??_C@_01LIIJDEN@?$AN?$AA@", "`string'");
        expect("??_C@_01BCJDKCAM@?$AM?$AA@", "`string'");
        expect("??_C@_01FNNCDEML@?$AL?$AA@", "`string'");
        expect("??_C@_01EEMJAFIK@?6?$AA@", "`string'");
        expect("??_C@_01GPOEFGEJ@?7?$AA@", "`string'");
        expect("??_C@_01HGPPGHAI@?$AI?$AA@", "`string'");
        expect("??_C@_01PBGHHLMH@?$AH?$AA@", "`string'");
        expect("??_C@_01OIHMEKIG@?$AG?$AA@", "`string'");
        expect("??_C@_01MDFBBJEF@?$AF?$AA@", "`string'");
        expect("??_C@_01NKEKCIAE@?$AE?$AA@", "`string'");
        expect("??_C@_01JFALLOMD@?$AD?$AA@", "`string'");
        expect("??_C@_01IMBAIPIC@?$AC?$AA@", "`string'");
        expect("??_C@_01KHDNNMEB@?$AB?$AA@", "`string'");
        expect("??_C@_01LOCGONAA@?$AA?$AA@", "`string'");

        // Wide characters.
        expect("??_C@_13KDLDGPGJ@?$AA?7?$AA?$AA@", "`string'");
        expect("??_C@_13LBAGMAIH@?$AA?6?$AA?$AA@", "`string'");
        expect("??_C@_13JLKKHOC@?$AA?$AL?$AA?$AA@", "`string'");
        expect("??_C@_13HOIJIPNN@?$AA?5?$AA?$AA@", "`string'");
        expect("??_C@_13MGDFOILI@?$AA?$CB?$AA?$AA@", "`string'");
        expect("??_C@_13NEIAEHFG@?$AA?$CC?$AA?$AA@", "`string'");
        expect("??_C@_13GMDMCADD@?$AA?$CD?$AA?$AA@", "`string'");
        expect("??_C@_13PBOLBIIK@?$AA$?$AA?$AA@", "`string'");
        expect("??_C@_13EJFHHPOP@?$AA?$CF?$AA?$AA@", "`string'");
        expect("??_C@_13FLOCNAAB@?$AA?$CG?$AA?$AA@", "`string'");
        expect("??_C@_13ODFOLHGE@?$AA?8?$AA?$AA@", "`string'");
        expect("??_C@_13LLDNKHDC@?$AA?$CI?$AA?$AA@", "`string'");
        expect("??_C@_13DIBMAFH@?$AA?$CJ?$AA?$AA@", "`string'");
        expect("??_C@_13BBDEGPLJ@?$AA?$CK?$AA?$AA@", "`string'");
        expect("??_C@_13KJIIAINM@?$AA?$CL?$AA?$AA@", "`string'");
        expect("??_C@_13DEFPDAGF@?$AA?0?$AA?$AA@", "`string'");
        expect("??_C@_13IMODFHAA@?$AA?9?$AA?$AA@", "`string'");
        expect("??_C@_13JOFGPIOO@?$AA?4?$AA?$AA@", "`string'");
        expect("??_C@_13CGOKJPIL@?$AA?1?$AA?$AA@", "`string'");
        expect("??_C@_13COJANIEC@?$AA0?$AA?$AA@", "`string'");
        expect("??_C@_13JGCMLPCH@?$AA1?$AA?$AA@", "`string'");
        expect("??_C@_13IEJJBAMJ@?$AA2?$AA?$AA@", "`string'");
        expect("??_C@_13DMCFHHKM@?$AA3?$AA?$AA@", "`string'");
        expect("??_C@_13KBPCEPBF@?$AA4?$AA?$AA@", "`string'");
        expect("??_C@_13BJEOCIHA@?$AA5?$AA?$AA@", "`string'");
        expect("??_C@_13LPLIHJO@?$AA6?$AA?$AA@", "`string'");
        expect("??_C@_13LDEHOAPL@?$AA7?$AA?$AA@", "`string'");
        expect("??_C@_13OLCEPAKN@?$AA8?$AA?$AA@", "`string'");
        expect("??_C@_13FDJIJHMI@?$AA9?$AA?$AA@", "`string'");
        expect("??_C@_13EBCNDICG@?$AA?3?$AA?$AA@", "`string'");
        expect("??_C@_13PJJBFPED@?$AA?$DL?$AA?$AA@", "`string'");
        expect("??_C@_13GEEGGHPK@?$AA?$DM?$AA?$AA@", "`string'");
        expect("??_C@_13NMPKAAJP@?$AA?$DN?$AA?$AA@", "`string'");
        expect("??_C@_13MOEPKPHB@?$AA?$DO?$AA?$AA@", "`string'");
        expect("??_C@_13HGPDMIBE@?$AA?$DP?$AA?$AA@", "`string'");
        expect("??_C@_13EFKPHINO@?$AA?$EA?$AA?$AA@", "`string'");
        expect("??_C@_13PNBDBPLL@?$AAA?$AA?$AA@", "`string'");
        expect("??_C@_13OPKGLAFF@?$AAB?$AA?$AA@", "`string'");
        expect("??_C@_13FHBKNHDA@?$AAC?$AA?$AA@", "`string'");
        expect("??_C@_13MKMNOPIJ@?$AAD?$AA?$AA@", "`string'");
        expect("??_C@_13HCHBIIOM@?$AAE?$AA?$AA@", "`string'");
        expect("??_C@_13GAMECHAC@?$AAF?$AA?$AA@", "`string'");
        expect("??_C@_13NIHIEAGH@?$AAG?$AA?$AA@", "`string'");
        expect("??_C@_13IABLFADB@?$AAH?$AA?$AA@", "`string'");
        expect("??_C@_13DIKHDHFE@?$AAI?$AA?$AA@", "`string'");
        expect("??_C@_13CKBCJILK@?$AAJ?$AA?$AA@", "`string'");
        expect("??_C@_13JCKOPPNP@?$AAK?$AA?$AA@", "`string'");
        expect("??_C@_13PHJMHGG@?$AAL?$AA?$AA@", "`string'");
        expect("??_C@_13LHMFKAAD@?$AAM?$AA?$AA@", "`string'");
        expect("??_C@_13KFHAAPON@?$AAN?$AA?$AA@", "`string'");
        expect("??_C@_13BNMMGIII@?$AAO?$AA?$AA@", "`string'");
        expect("??_C@_13BFLGCPEB@?$AAP?$AA?$AA@", "`string'");
        expect("??_C@_13KNAKEICE@?$AAQ?$AA?$AA@", "`string'");
        expect("??_C@_13LPLPOHMK@?$AAR?$AA?$AA@", "`string'");
        expect("??_C@_13HADIAKP@?$AAS?$AA?$AA@", "`string'");
        expect("??_C@_13JKNELIBG@?$AAT?$AA?$AA@", "`string'");
        expect("??_C@_13CCGINPHD@?$AAU?$AA?$AA@", "`string'");
        expect("??_C@_13DANNHAJN@?$AAV?$AA?$AA@", "`string'");
        expect("??_C@_13IIGBBHPI@?$AAW?$AA?$AA@", "`string'");
        expect("??_C@_13NAACAHKO@?$AAX?$AA?$AA@", "`string'");
        expect("??_C@_13GILOGAML@?$AAY?$AA?$AA@", "`string'");
        expect("??_C@_13HKALMPCF@?$AAZ?$AA?$AA@", "`string'");
        expect("??_C@_13MCLHKIEA@?$AA?$FL?$AA?$AA@", "`string'");
        expect("??_C@_13FPGAJAPJ@?$AA?2?$AA?$AA@", "`string'");
        expect("??_C@_13OHNMPHJM@?$AA?$FN?$AA?$AA@", "`string'");
        expect("??_C@_13PFGJFIHC@?$AA?$FO?$AA?$AA@", "`string'");
        expect("??_C@_13ENNFDPBH@?$AA_?$AA?$AA@", "`string'");
        expect("??_C@_13OFJNNHOA@?$AA?$GA?$AA?$AA@", "`string'");
        expect("??_C@_13FNCBLAIF@?$AAa?$AA?$AA@", "`string'");
        expect("??_C@_13EPJEBPGL@?$AAb?$AA?$AA@", "`string'");
        expect("??_C@_13PHCIHIAO@?$AAc?$AA?$AA@", "`string'");
        expect("??_C@_13GKPPEALH@?$AAd?$AA?$AA@", "`string'");
        expect("??_C@_13NCEDCHNC@?$AAe?$AA?$AA@", "`string'");
        expect("??_C@_13MAPGIIDM@?$AAf?$AA?$AA@", "`string'");
        expect("??_C@_13HIEKOPFJ@?$AAg?$AA?$AA@", "`string'");
        expect("??_C@_13CACJPPAP@?$AAh?$AA?$AA@", "`string'");
        expect("??_C@_13JIJFJIGK@?$AAi?$AA?$AA@", "`string'");
        expect("??_C@_13IKCADHIE@?$AAj?$AA?$AA@", "`string'");
        expect("??_C@_13DCJMFAOB@?$AAk?$AA?$AA@", "`string'");
        expect("??_C@_13KPELGIFI@?$AAl?$AA?$AA@", "`string'");
        expect("??_C@_13BHPHAPDN@?$AAm?$AA?$AA@", "`string'");
        expect("??_C@_13FECKAND@?$AAn?$AA?$AA@", "`string'");
        expect("??_C@_13LNPOMHLG@?$AAo?$AA?$AA@", "`string'");
        expect("??_C@_13LFIEIAHP@?$AAp?$AA?$AA@", "`string'");
        expect("??_C@_13NDIOHBK@?$AAq?$AA?$AA@", "`string'");
        expect("??_C@_13BPINEIPE@?$AAr?$AA?$AA@", "`string'");
        expect("??_C@_13KHDBCPJB@?$AAs?$AA?$AA@", "`string'");
        expect("??_C@_13DKOGBHCI@?$AAt?$AA?$AA@", "`string'");
        expect("??_C@_13ICFKHAEN@?$AAu?$AA?$AA@", "`string'");
        expect("??_C@_13JAOPNPKD@?$AAv?$AA?$AA@", "`string'");
        expect("??_C@_13CIFDLIMG@?$AAw?$AA?$AA@", "`string'");
        expect("??_C@_13HADAKIJA@?$AAx?$AA?$AA@", "`string'");
        expect("??_C@_13MIIMMPPF@?$AAy?$AA?$AA@", "`string'");
        expect("??_C@_13NKDJGABL@?$AAz?$AA?$AA@", "`string'");
        expect("??_C@_13GCIFAHHO@?$AA?$HL?$AA?$AA@", "`string'");
        expect("??_C@_13PPFCDPMH@?$AA?$HM?$AA?$AA@", "`string'");
        expect("??_C@_13EHOOFIKC@?$AA?$HN?$AA?$AA@", "`string'");
        expect("??_C@_13FFFLPHEM@?$AA?$HO?$AA?$AA@", "`string'");

        // Tests for maximum string length
        expect(
            "??_C@_0CF@LABBIIMO@012345678901234567890123456789AB@",
            "`string'",
        );
        expect("??_C@_1EK@KFPEBLPK@?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AA0?$AA1?$AA2?$AA3?$AA4?$AA5?$AA6?$AA7?$AA8?$AA9?$AAA?$AAB@", "`string'");
        // Unicode character.
        expect("??_C@_13IIHIAFKH@?W?$PP?$AA?$AA@", "`string'");
        // u8/u/U literal strings.
        expect("??_C@_02PCEFGMJL@hi?$AA@", "`string'");
        expect("??_C@_05OMLEGLOC@h?$AAi?$AA?$AA?$AA@", "`string'");
        expect(
            "??_C@_0M@GFNAJIPG@h?$AA?$AA?$AAi?$AA?$AA?$AA?$AA?$AA?$AA?$AA@",
            "`string'",
        );
    }

    #[test]
    fn upstream_tests() {
        let expect = |input, reference| {
            expect_with_flags(input, reference, 0x0);
        };
        expect("?x@@3HA", "int x");
        expect("?x@@3PEAHEA", "int *x");
        expect("?x@@3PEAPEAHEA", "int * *x");
        expect("?x@@3PEAY02HEA", "int (*x)[3]");
        expect("?x@@3PEAY124HEA", "int (*x)[3][5]");
        expect("?x@@3PEAY02$$CBHEA", "int const (*x)[3]");
        expect("?x@@3PEAEEA", "unsigned char *x");
        expect("?x@@3PEAY1NKM@5HEA", "int (*x)[3500][6]");
        expect("?x@@YAXMH@Z", "void __cdecl x(float,int)");
        expect("?x@@YAXMH@Z", "void __cdecl x(float,int)");
        expect("?x@@3P6AHMNH@ZEA", "int __cdecl (*x)(float,double,int)");
        expect(
            "?x@@3P6AHP6AHM@ZN@ZEA",
            "int __cdecl (*x)(int __cdecl (*)(float),double)",
        );
        expect(
            "?x@@3P6AHP6AHM@Z0@ZEA",
            "int __cdecl (*x)(int __cdecl (*)(float),int __cdecl (*)(float))",
        );

        expect("?x@ns@@3HA", "int ns::x");

        // Microsoft's undname returns "int const * const x" for this symbol.
        // I believe it's their bug.
        expect("?x@@3PEBHEB", "int const *x");

        expect("?x@@3QEAHEB", "int * const x");
        expect("?x@@3QEBHEB", "int const * const x");

        expect("?x@@3AEBHEB", "int const & x");

        expect("?x@@3PEAUty@@EA", "struct ty *x");
        expect("?x@@3PEATty@@EA", "union ty *x");
        expect("?x@@3PEAUty@@EA", "struct ty *x");
        expect("?x@@3PEAW4ty@@EA", "enum ty *x");
        expect("?x@@3PEAVty@@EA", "class ty *x");

        expect("?x@@3PEAV?$tmpl@H@@EA", "class tmpl<int> *x");
        expect("?x@@3PEAU?$tmpl@H@@EA", "struct tmpl<int> *x");
        expect("?x@@3PEAT?$tmpl@H@@EA", "union tmpl<int> *x");
        expect("?instance@@3Vklass@@A", "class klass instance");
        expect(
            "?instance$initializer$@@3P6AXXZEA",
            "void __cdecl (*instance$initializer$)(void)",
        );
        expect("??0klass@@QEAA@XZ", "public: __cdecl klass::klass(void)");
        expect("??1klass@@QEAA@XZ", "public: __cdecl klass::~klass(void)");
        expect(
            "?x@@YAHPEAVklass@@AEAV1@@Z",
            "int __cdecl x(class klass *,class klass &)",
        );
        expect(
            "?x@ns@@3PEAV?$klass@HH@1@EA",
            "class ns::klass<int,int> *ns::x",
        );
        expect(
            "?fn@?$klass@H@ns@@QEBAIXZ",
            "public: unsigned int __cdecl ns::klass<int>::fn(void)const ",
        );

        expect(
            "??4klass@@QEAAAEBV0@AEBV0@@Z",
            "public: class klass const & __cdecl klass::operator=(class klass const &)",
        );
        expect(
            "??7klass@@QEAA_NXZ",
            "public: bool __cdecl klass::operator!(void)",
        );
        expect(
            "??8klass@@QEAA_NAEBV0@@Z",
            "public: bool __cdecl klass::operator==(class klass const &)",
        );
        expect(
            "??9klass@@QEAA_NAEBV0@@Z",
            "public: bool __cdecl klass::operator!=(class klass const &)",
        );
        expect(
            "??Aklass@@QEAAH_K@Z",
            "public: int __cdecl klass::operator[](uint64_t)",
        );
        expect(
            "??Cklass@@QEAAHXZ",
            "public: int __cdecl klass::operator->(void)",
        );
        expect(
            "??Dklass@@QEAAHXZ",
            "public: int __cdecl klass::operator*(void)",
        );
        expect(
            "??Eklass@@QEAAHXZ",
            "public: int __cdecl klass::operator++(void)",
        );
        expect(
            "??Eklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator++(int)",
        );
        expect(
            "??Fklass@@QEAAHXZ",
            "public: int __cdecl klass::operator--(void)",
        );
        expect(
            "??Fklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator--(int)",
        );
        expect(
            "??Hklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator+(int)",
        );
        expect(
            "??Gklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator-(int)",
        );
        expect(
            "??Iklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator&(int)",
        );
        expect(
            "??Jklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator->*(int)",
        );
        expect(
            "??Kklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator/(int)",
        );
        expect(
            "??Mklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator<(int)",
        );
        expect(
            "??Nklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator<=(int)",
        );
        expect(
            "??Oklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator>(int)",
        );
        expect(
            "??Pklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator>=(int)",
        );
        expect(
            "??Qklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator,(int)",
        );
        expect(
            "??Rklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator()(int)",
        );
        expect(
            "??Sklass@@QEAAHXZ",
            "public: int __cdecl klass::operator~(void)",
        );
        expect(
            "??Tklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator^(int)",
        );
        expect(
            "??Uklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator|(int)",
        );
        expect(
            "??Vklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator&&(int)",
        );
        expect(
            "??Wklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator||(int)",
        );
        expect(
            "??Xklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator*=(int)",
        );
        expect(
            "??Yklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator+=(int)",
        );
        expect(
            "??Zklass@@QEAAHH@Z",
            "public: int __cdecl klass::operator-=(int)",
        );
        expect(
            "??_0klass@@QEAAHH@Z",
            "public: int __cdecl klass::operator/=(int)",
        );
        expect(
            "??_1klass@@QEAAHH@Z",
            "public: int __cdecl klass::operator%=(int)",
        );
        expect(
            "??_2klass@@QEAAHH@Z",
            "public: int __cdecl klass::operator>>=(int)",
        );
        expect(
            "??_3klass@@QEAAHH@Z",
            "public: int __cdecl klass::operator<<=(int)",
        );
        expect(
            "??_6klass@@QEAAHH@Z",
            "public: int __cdecl klass::operator^=(int)",
        );
        expect(
            "??6@YAAEBVklass@@AEBV0@H@Z",
            "class klass const & __cdecl operator<<(class klass const &,int)",
        );
        expect(
            "??5@YAAEBVklass@@AEBV0@_K@Z",
            "class klass const & __cdecl operator>>(class klass const &,uint64_t)",
        );
        expect(
            "??2@YAPEAX_KAEAVklass@@@Z",
            "void * __cdecl operator new(uint64_t,class klass &)",
        );
        expect(
            "??_U@YAPEAX_KAEAVklass@@@Z",
            "void * __cdecl operator new[](uint64_t,class klass &)",
        );
        expect(
            "??3@YAXPEAXAEAVklass@@@Z",
            "void __cdecl operator delete(void *,class klass &)",
        );
        expect(
            "??_V@YAXPEAXAEAVklass@@@Z",
            "void __cdecl operator delete[](void *,class klass &)",
        );
    }
}
