#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let initial =
    inputFile "16" string
    |> Seq.exactlyOne

let pattern = [|0;1;0;-1|]
let patternValue outPos inPos = pattern.[((inPos + 1) / (outPos + 1)) % 4]

// ===================================================================================================================
// 2019.16 A
// ===================================================================================================================

let solveA count (phase:string)=
    let mutable phase = phase  |> Seq.map ((sprintf "%c") >> int) |> Seq.toList
    let mutable count = count

    while count > 0 do
        // solution "16a.1" "24176176" (phase.[0 .. 7] |> List.map ((sprintf "%i") >> char) |> Array.ofList |> cstring)
        phase <-
            Seq.init (phase.Length) (fun outPos ->
                phase |> List.mapi (fun inPos digit ->
                    let patternValue = patternValue outPos inPos 
                    digit * patternValue
                ) |> List.sum |> fun i -> abs (i % 10)
            ) |> Seq.toList
        count <- count - 1
    
    phase.[0 .. 7] |> List.map ((fun i -> sprintf "%i" i) >> char) |> Array.ofList |> cstring

solveA 100 "80871224585914546619083218645595"
|> solution "16a.1" "24176176"
solveA 100 "19617804207202209144916044189917"
|> solution "16a.1" "73745418"
solveA 100 "69317163492948606335995924319873"
|> solution "16a.1" "52432133"
solveA 100 initial
|> solution "16a" "67481260"

// ===================================================================================================================
// 2019.16 B
// ===================================================================================================================

let solveB _ _ = "0"

solveB 100 "03036732577212944063491565474664"
|> solution "16a.1" "84462026"
solveB 100 "02935109699940807407585447034323"
|> solution "16a.1" "78725270"
solveB 100 "03081770884921959731165446850517"
|> solution "16a.1" "53553731"
solveB initial
|> solution "16b" "0"