open System
//open Dna
open System
open DnaAlign
open std

let q = "TGACTGATCTACTGACTGATGCTGACTGACTGACTGACCCAACTGACTGACGTTACTG".ToCharArray()
let s = "TGACTGATCTACTACTGATaCTGACTGACcGACTGACCCAACTGACTGACGTTACTG".ToCharArray()

[<EntryPoint>]
let main argv =
    //printfn "Hello from F#"
    Console.WriteLine("Hello from F# (via fable-> rust)")
    let align = gLocal false porechop q s

    Console.WriteLine($"Alignment: {align.QAlign}")
    Console.WriteLine($"Alignment: {align.SAlign}")
    //printfn "Whatever"
    0
