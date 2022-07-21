open System
open Amyris.Dna
open System
open Dmx.BoomSeq.Shared.DnaAlign


let q = Dna "TGACTGATCTACTGACTGATGCTGACTGACTGACTGACCCAACTGACTGACGTTACTG"
let s = Dna "TGACTGATCTACTACTGATaCTGACTGACcGACTGACCCAACTGACTGACGTTACTG"

[<EntryPoint>]
let main argv =
    //printfn "Hello from F#"
    Console.WriteLine("Hello from F# (via fable-> rust)")
    let align = gLocal false porechop q s

    Console.WriteLine($"Alignment: {align.QAlign}")
    Console.WriteLine($"Alignment: {align.SAlign}")
    //printfn "Whatever"
    0
