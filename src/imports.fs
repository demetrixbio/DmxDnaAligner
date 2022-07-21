module Imports

open std
open Fable.Core

module Native =
    [<Import("String_::string", "fable_library_rust")>]
    let toString (v: String inref): string = nativeOnly

    [<Import("Native_::array", "fable_library_rust")>]
    let toArray (v: Vec<'T>): 'T[] = nativeOnly

module Imports =
    [<Emit("()")>]
    let emitUnit args: unit = nativeOnly

    let inline private import selector path =
        emitUnit (Rust.import selector path)

    // this adds some imports used in the bindings above
    let imports(): unit =
        import "Native_::array" "fable_library_rust"
        import "String_::string" "fable_library_rust"
        import "std::io::BufRead" ""
