extern crate msvc_demangler;

use std::env;
use std::io;
use std::io::BufRead;

fn main() {
    let args = env::args();

    let print_demangled = |sym: &str| {
        let demangled = msvc_demangler::demangle(&sym, msvc_demangler::DemangleFlags::COMPLETE);
        match demangled {
            Ok(ref string) => println!("{}", string),
            _ => println!("{}", sym),
        }
    };

    if args.len() == 1 {
        let stdin = io::stdin();
        let handle = stdin.lock();

        for line in handle.lines() {
            match line {
                Ok(line) => print_demangled(&line),
                _ => continue,
            }
        }
        return;
    }

    for (i, arg) in env::args().enumerate() {
        if i == 0 {
            continue;
        }

        print_demangled(&arg);
    }
}
