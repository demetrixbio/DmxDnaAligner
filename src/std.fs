// mini Rust std lib bindings (just an example, not guaranteed to be stable)
module std

open Fable.Core

[<AutoOpen>]
module string =
    [<Erase; Emit("String")>]
    type String =
        [<Emit("String::from($0.as_ref())")>]
        static member from(s: string): String = nativeOnly

[<AutoOpen>]
module vec =
    [<Erase; Emit("Vec")>]
    type Vec<'T> =
        [<Emit("Vec::new()")>]
        static member new_(): Vec<'T> = nativeOnly

module io =
    [<Erase; Emit("std::io::Error")>]
    type Error =
        [<Emit("std::io::Error::last_os_error()")>]
        static member last_os_error(): Error = nativeOnly
        [<Emit("$0.raw_os_error()")>]
        member _.raw_os_error(): int option = nativeOnly

    [<Erase; Emit("std::io::BufReader")>]
    type BufReader<'T> =
        [<Emit("std::io::BufReader::new($0)")>]
        static member new_(inner: 'T): BufReader<'T> = nativeOnly

        [<Emit("$0.lines().map(|res| res.map(|s| string(&s))).collect::<Result<Vec<_>, _>>().map(array)")>]
        member _.lines(): Result<string[], Error> = nativeOnly

module fs =
    [<Erase; Emit("std::fs::File")>]
    type File =
        [<Emit("std::fs::File::open($0.as_ref())")>]
        static member open_(path: string): Result<File, io.Error> = nativeOnly

    [<Emit("std::fs::read($0.as_ref()).map(array)")>]
    let read(path: string): Result<byte[], io.Error> = nativeOnly

    [<Emit("std::fs::read_to_string($0.as_ref()).map(|s| string(&s))")>]
    let read_to_string(path: string): Result<string, io.Error> = nativeOnly
