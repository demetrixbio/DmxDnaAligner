git clean -fdX
#dotnet run -c Release --project ../Fable/src/Fable.Cli -- --lang Rust --outDir . --noCache
dotnet tool restore
dotnet fable --lang rust --outDir .
cargo build
