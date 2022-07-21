module DnaAlign

// open Dna
open System
open utils
open std
open Imports

let array2DInit n m f =
    let arr = Array.init n (fun i ->
        Array.init m (fun j ->
            f i j
        )
    )
    in arr

type Alignment = {  QAlign: string
                    SAlign: string
                    SFr: int
                    STo: int
                    Score: int }

//let indelPen = -1
type Penalties =
    {   Match: int
        Mismatch: int
        GapOpen: int
        GapExtend: int
        NScore: int }

type From =
    | Above = 0
    | Left = 1
    | UpLeft = 2
    | Start = 3
    | Undefined = 4


(*
let indelStartPen = -5

let indelExtensionPen = -2
let matchScore = 3
let nScore = 0
let mismatchScore = -6
*)

let porechop =
    {   Match = 3
        Mismatch = -6
        GapOpen = -5
        GapExtend = -2
        NScore = 0 }

let blast =
    {   Match = 5
        Mismatch = -4
        GapOpen = -8
        GapExtend = -6
        NScore = 0 }

(*
    porechop scoring: (default: match = 3, mismatch = -6, gap open = -5, gap extend = -2).

    https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4835549/
    default parameters from BLAST: match=5, mismatch=−4, gapopen=−8 and gapextend=−6).

    https://denbi-nanopore-training-course.readthedocs.io/en/latest/read_qc/ReadMapping.html
    match = 5, mismatch = -4 open=-8 extend = -6
*)


/// Search for full length query in a subject sequence
let gLocal log (pen: Penalties) (query:char []) (subject: char[]) =
    let qL = query.Length
    let sL = subject.Length

    if log then
        (*

        printfn "alignScan:  %s" (arr2seq query)
        printfn "            %s" (arr2seq subject)

        printfn
            "align:         %s"
            (String.Join(
                "|",
                [| yield "   X  "
                   for i in 1 .. qL do
                        yield sprintf "   %c  " (query.[i - 1]) |]
            ))
        *)
        ()
    // Scoring space and from storage
    // note - 1 empty row above starting row so sL+1 rows
    let scores = 
        array2DInit
            (sL + 1)
            (qL + 1)
            (fun row col ->
                if col = 0 then
                    0
                else
                    pen.GapOpen + (col - 1) * pen.GapExtend)

    let from =
        array2DInit
            (sL + 1)
            (qL + 1)
            (fun row col ->
                if col = 0 then From.Start
                elif row = 0 then From.Left
                else From.Undefined)

    let inline better aIsExtend aScore bIsExtend bScore =
        (aScore > bScore)
        || (aScore = bScore && aIsExtend && (not bIsExtend))

    for row in 1 .. sL do
        //                            \  |
        // score each cell as max of   \ v
        //                          -->  *
        let prevRow = row - 1
        let thisRow = row // due to spare row

        for i in 1 .. qL do
            let leftGapExtend = from.[thisRow][i - 1] = From.Left

            let fromLeft =
                scores.[thisRow][i - 1]
                + (if leftGapExtend then
                        pen.GapExtend
                    else
                        pen.GapOpen)

            let aboveGapExtend = from.[prevRow][i] = From.Above

            let fromAbove =
                scores.[prevRow][i]
                + (if aboveGapExtend then
                        pen.GapExtend
                    else
                        pen.GapOpen)

            let q = query.[i - 1]
            let s = subject.[row - 1]

            let diag =
                scores.[prevRow][i - 1]
                + (if q = 'N' || s = 'N' then pen.NScore
                    else if q = s then pen.Match
                    else pen.Mismatch)
            // if fromLeft > fromAbove && fromLeft > diag then
            if (better leftGapExtend fromLeft aboveGapExtend fromAbove)
                && (better leftGapExtend fromLeft false diag) then
                scores.[thisRow][i] <- fromLeft
                from.[thisRow][i] <- From.Left
            // elif fromAbove > fromLeft && fromAbove > diag then
            elif (better aboveGapExtend fromAbove leftGapExtend fromLeft)
                    && better aboveGapExtend fromAbove false diag then
                scores.[thisRow][i] <- fromAbove
                from.[thisRow][i] <- From.Above
            else
                scores.[thisRow][i] <- diag
                from.[thisRow][i] <- From.UpLeft

        (*

        if log then
            printfn
                "align: %4d %c  %s"
                row
                (subject.[row - 1])
                (String.Join(
                    "|",
                    [| for i in 0 .. qL ->
                            sprintf
                                "%4d %c"
                                (scores.[thisRow][i])
                                (match from.[thisRow][i] with
                                | From.UpLeft -> '\\'
                                | From.Left -> '<'
                                | From.Above -> '^'
                                | From.Undefined -> '?'
                                | From.Start -> '!'
                                | _ -> 'X') |]
                ))
        *)
    // Determine best alignment
    let maxScore, maxScoreRow =
        [| for i in 0 .. sL -> scores.[i][qL], i |]
        |> Array.max

    let rec emitAlign row col res =
        (*

        if log then
            printfn
                "align: tb row=%d col=%d score=%d origin=%A q=%c s=%c"
                row
                col
                (scores.[row][col])
                (from.[row][col])
                (if col > 0 then query.[col - 1] else '?')
                (if row > 0 then
                        subject.[row - 1]
                    else
                        '?')
        *)

        match from.[row][col] with
        | From.Left -> emitAlign row (col - 1) ((query.[col - 1], '-') :: res)
        | From.UpLeft -> emitAlign (row - 1) (col - 1) ((query.[col - 1], subject.[row - 1]) :: res)
        | From.Above -> emitAlign (row - 1) col (('-', subject.[row - 1]) :: res)
        | From.Start ->
            let q, s = res |> List.unzip

            {   QAlign = String(q |> Array.ofList)
                SAlign = String(s |> Array.ofList)
                SFr = row
                STo = maxScoreRow - 1
                Score = maxScore }
        | From.Undefined ->
#if FABLE_COMPILER
            System.Console.WriteLine(query |>arr2seq)
            System.Console.WriteLine(subject |>arr2seq)
#else
            use outF =
                new System.IO.StreamWriter("alignerror.txt")

            outF.WriteLine(query |>arr2seq)
            outF.WriteLine(subject |>arr2seq)
            outF.Close()
#endif

            failwith "XXX"
            //failwithf
            //    "ERROR: at cell row=%d,col=%d encountered Undefined From value writing alignerror.txt with inputs"
            //    row
            //    col
        | _ ->
            failwith "XXX"
            //failwithf
            //    "ERROR: at cell row=%d,col=%d encountered illegal value From value writing alignerror.txt with inputs"
            //    row
            //    col

    emitAlign maxScoreRow (qL) []


let testOne (q: char[]) (s: char[]) =
    //let s = Dna "CGGTAGTGCTTCGTTCGATTTTGTATTGCTGAACATTCTTCATACACATAATGGCCAGTAACACTCCAATTCTGGGTGATTTACGTACATATAGATgagtAGCTAGTTGAGATTACTAAGGATTTTTGTTAAATGTGATTCTTATCTATCTGGCACTTGGATTGAGAATATCTTTACTATCGGTATACTAAACTTGCCGACCCTGGAGCAATTACGCGTACTACTACAGCGTTAATTATCTGAAACTGCCTTGGTTGGCCAAGTGGGCCGTACGAATTTGTTCCAAGAGCGGTTAGCTTAGTTATTCATATAAGGGGTAAACACTATTTCTGCTGACGATACGCACAGAACAGAGGAACCGATCCAAATTCTGTCTCCCTTTTAACCTTCTTCAGTGAAAAGAGCGTGTATTCCTTCTTTAAACGATTTAAATAATTAAGAATTGTAGAACCTTGGCTCTGGAACACAAATTTATAGAAGAGGTTTGAGAACGTCAATTTAATAGATACTAAATTTACATTACTTCCGGGATTTTTCATAATTAGATAAGCATGCTTTGCTTATCTGGTATGAAAATCCCAGCTGCTCTTGAGCCAGTCATCTGCTATACTTCGCAGAACTGTTGTTCAGAGGCAGGTTCTATTGCTGGTGTCGTCGCTGAGAGGAATGCGCAGCTCGCGGGTTAAAGAACTGAAGTCATTCCTTTAGTTTACCATTGCGTTTGATTTCACACCGCCGAAGGCGGCCCAATATCTTGGGATGAGTAAACGGCTGCGATTTCAGGAATGATGGCTGGACCGTATTATTACGCGTACTATCTAGACCGAGTTTAGTACCGACTGGTAAGATATTCTCGAGTGCCGAATGGGCGTCACATTTAACAAATCGCAGTATCTCAGCAACTACTCATCCCTATGTACAGAATGAGTGTTTGGCACCAATATGTGCGTAGCGTTGCGCGATACGTA"
    //let q = Dna "GCAGCCGCGGGT"

    // tests leading space required in subject
    // let q = Dna "GCATCTCTGCCG"
    // let s = Dna "CATTCTGTGCATGGAG"

    // tests pathologically short subject
    //let q = Dna "CCAGTAACACTCCAATTCTGGGTGATTTAC"
    //let s = Dna "C"


    let result = gLocal true blast q s
    (*

    printfn "q    =%s" (arr2seq q)
    printfn "Score=%d" result.Score
    printfn "qa   =%s" result.QAlign
    printfn "sa   =%s" result.SAlign
    printfn "sf   =%d" result.SFr
    printfn "st   =%d" result.STo
    printfn "scut =%s" (s.[result.SFr..result.STo] |> arr2seq)
    *)
    ()

let test () =

    (*
    let s = Dna "ATCTTCGGTCCTTCGCATAAA"

    let S1931RC = Dna "GGTCCTTCGCAT"
    let S1187RC = Dna "GTCGAAGGCTTC"
    // let S1187 = Dna "GAAGCCTTCGAC"

    printfn "Wrong S1187 barcode"
    testOne S1187RC s

    printfn "True S1931 barcode"
    testOne S1931RC s
    *)
    let q =
        ("CCAATATGTGCATGAAGAATGTTC".Replace("-", "")).ToCharArray()

    let s =
        ("CCAATATGTGC-------ATG---".Replace("-", "")).ToCharArray()


    ()