use std::env;
extern crate msvc_demangler;
use msvc_demangler::*;
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("{} <symbol>", args[0]);
        std::process::exit(1);
    }

    match demangle(&args[1], DemangleFlags::COMPLETE) {
        Ok(s) => {
            println!("{}", s);
        }
        Err(err) => {
            eprintln!("error: {:?}", err);
            std::process::exit(1);
        }
    }
}
