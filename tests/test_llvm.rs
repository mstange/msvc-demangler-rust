extern crate msvc_demangler;

use msvc_demangler::{parse, serialize, DemangleFlags};
use std::iter;

#[derive(Debug)]
pub struct TestCase<'a> {
    mangled: &'a str,
    demangled_ref: &'a str,
    not_invalid: bool,
}

#[derive(Debug)]
enum LineRule<'a> {
    Input(&'a str),
    Check(&'a str),
    CheckNotInvalid,
}

fn parse_cases<'a, I: Iterator<Item = &'a str>>(i: I) -> impl Iterator<Item = TestCase<'a>> {
    let mut rule_iter = i.filter_map(|item| {
        let item = item.trim();
        if item.is_empty() {
            None
        } else if item.starts_with("; RUN: ") {
            assert!(item.contains("llvm-undname"));
            None
        } else if item == "; CHECK-NOT: Invalid mangled name" {
            Some(LineRule::CheckNotInvalid)
        } else if item.starts_with("; CHECK-NOT: ") {
            panic!("unsupported rule: {}", &item[2..]);
        } else if let Some(item) = item.strip_prefix("; CHECK: ") {
            Some(LineRule::Check(item))
        } else if item.starts_with(';') {
            None
        } else {
            Some(LineRule::Input(item))
        }
    });

    let mut not_invalid = false;
    iter::from_fn(move || loop {
        match rule_iter.next() {
            None => return None,
            Some(LineRule::CheckNotInvalid) => {
                not_invalid = true;
            }
            Some(LineRule::Input(input)) =>
            {
                #[allow(clippy::while_let_on_iterator)]
                while let Some(next) = rule_iter.next() {
                    match next {
                        LineRule::CheckNotInvalid => {
                            panic!("not invalid at unexpected position");
                        }
                        LineRule::Check(check) => {
                            return Some(TestCase {
                                mangled: input,
                                demangled_ref: check,
                                not_invalid,
                            });
                        }
                        LineRule::Input(_) => {
                            panic!("multi line input unsupported");
                        }
                    }
                }
            }
            Some(LineRule::Check(check)) => {
                panic!("unexpected check: {}", check);
            }
        }
    })
}

macro_rules! llvm_test {
    ($filename:expr) => {{
        let rules = include_str!($filename);
        for case in parse_cases(rules.lines()) {
            if case.not_invalid {
                let parsed = dbg!(parse(case.mangled).unwrap());
                let demangled = serialize(&parsed, DemangleFlags::llvm()).unwrap();
                println!("      mangled: {}", case.mangled);
                println!("demangled ref: {}", case.demangled_ref);
                println!("    demangled: {}", &demangled);
                // llvm is inconsistent with ctor vs constructor in a few cases
                let demangled_fuzzy = demangled
                    .replace("constructor", "ctor")
                    .replace("destructor", "dtor")
                    .replace("::`RTTI", " `RTTI")
                    .replace("> > > >", ">>>>")
                    .replace("> > >", ">>>")
                    .replace("> >", ">>");
                assert!(
                    demangled_fuzzy.contains(case.demangled_ref)
                        || demangled.contains(case.demangled_ref)
                );
            } else {
                panic!("not implemented");
            }
            println!();
        }
    }};
}

#[test]
fn test_llvm_ms_basic() {
    llvm_test!("llvm-cases/ms-basic.test");
}

#[test]
fn test_llvm_ms_operators() {
    llvm_test!("llvm-cases/ms-operators.test");
}

#[test]
fn test_llvm_ms_back_references() {
    llvm_test!("llvm-cases/ms-back-references.test");
}

#[test]
fn test_llvm_ms_windows() {
    llvm_test!("llvm-cases/ms-windows.test");
}

#[test]
fn test_llvm_cxx11() {
    llvm_test!("llvm-cases/ms-cxx11.test");
}
