extern crate msvc_demangler;

use std::env;
use std::io;
use std::io::BufRead;

fn main() {
    let mut args: Vec<_> = env::args().collect();
    args.remove(0);

    let verbose = if args.get(0).map(|x| x.as_str()) == Some("-v") {
        args.remove(0);
        true
    } else {
        false
    };

    let print_demangled = |sym: &str| {
        let parsed = match msvc_demangler::parse(&sym) {
            Ok(parsed) => parsed,
            Err(err) => {
                eprintln!("error: {}", err);
                println!("{}", sym);
                return;
            }
        };
        if verbose {
            eprintln!("{:#?}", &parsed);
        }
        let flags = msvc_demangler::DemangleFlags::llvm();
        let demangled = msvc_demangler::serialize(&parsed, flags);
        match demangled {
            Ok(ref string) => println!("{}", string),
            Err(err) => {
                eprintln!("error: {}", err);
                println!("{}", sym);
            }
        }
    };

    if args.is_empty() {
        let stdin = io::stdin();
        let handle = stdin.lock();

        for line in handle.lines() {
            match line {
                Ok(line) => print_demangled(&line),
                _ => continue,
            }
        }
    } else {
        for arg in args {
            print_demangled(&arg);
        }
    }
}
