# Keep current builds for now for faster builds
# git clean -fdX

# Use these lines if you want to build with a local fable version
# pushd ../Fable
# git checkout snake_island
# git pull
# ./build.sh library-rust
# popd
# dotnet run -c Release --project ../Fable/src/Fable.Cli -- --lang Rust --outDir . --noCache

# Building with Fable tool specificied in ./.config/dotnet-tools.json
dotnet tool restore

# dotnet fable --lang rust --outDir .
# cargo run

dotnet fable watch --lang rust --outDir . --runWatch cargo run
