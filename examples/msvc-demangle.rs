use std::env;
extern crate msvc_demangler_rust;
use msvc_demangler_rust::*;
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("{} <symbol>", args[0]);
        std::process::exit(1);
    }

    match demangle(&args[1], DemangleFlags::LotsOfWhitespace) {
        Ok(s) => {
            println!("{}", s);
        }
        Err(err) => {
            eprintln!("error: {:?}", err);
            std::process::exit(1);
        }
    }
}


