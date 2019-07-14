# msvc-demangler

msvc-demangler is a crate for Rust that can demangle C++ symbols which use
the MSVC mangling scheme.  These are emitted by the Microsoft C++ compiler
for Windows as well as some others.

## Example

```rust
use msvc_demangler;
let flags = msvc_demangler::DemangleFlags::llvm();
let result = msvc_demangler::demangle("??_0klass@@QEAAHH@Z", flags).unwrap();
println!("{}", result);
```

## Behavior

It's functionality is similar to `undname` on Windows and the underlying
`UnDecorateSymbolName` function.  Since Microsoft does not document the
mangling scheme this is likely not to be entirely accurate.  When unclear
the implementation tries to follow what LLVM does.

## License

This msvc-demangler is dual licensed under the MIT and the University of
Illinois Open Source Licenses.

License: MIT/NCSA
