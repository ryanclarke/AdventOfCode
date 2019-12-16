#load "utils/Base.fsx"
open Base

let initial =
    inputFile "16" string
    |> Seq.exactlyOne

let pattern = [|0;1;0;-1|]

// ===================================================================================================================
// 2019.16 A
// ===================================================================================================================

let solveA (phase:string)=
    let mutable phase = phase  |> Seq.map ((sprintf "%c") >> int) |> Seq.toList
    let mutable count = 100

    while count > 0 do
        phase <-
            Seq.init (phase.Length) (fun outPos ->
                phase |> List.mapi (fun inPos digit ->
                    let patternValue = pattern.[((inPos + 1) / (outPos + 1)) % 4] 
                    digit * patternValue
                ) |> List.sum |> fun i -> abs (i % 10)
            ) |> Seq.toList
        count <- count - 1
    
    phase.[0 .. 7] |> List.map ((fun i -> sprintf "%i" i) >> char) |> Array.ofList |> cstring

solveA "80871224585914546619083218645595"
|> solution "16a.1" "24176176"
solveA "19617804207202209144916044189917"
|> solution "16a.2" "73745418"
solveA "69317163492948606335995924319873"
|> solution "16a.3" "52432133"
solveA initial
|> solution "16a" "67481260"

// ===================================================================================================================
// 2019.16 B
// ===================================================================================================================

let solveB (phase:string) =
    let offset = phase.[0 .. 6] |> int
    let mutable phase = phase |> Seq.map ((sprintf "%c") >> int) |> Seq.toList
    phase <- Seq.init 10000 (fun _ -> phase) |> Seq.toList |> List.collect id
    phase <- phase.[offset..] |> List.rev
        
    let mutable count = 0

    while count < 100 do
        phase <- phase |> List.mapFold (fun s i -> (abs ((s + i) % 10)),(s + i)) 0 |> fst
        count <- count + 1
    
    phase |> List.rev |> fun l -> l.[0..7] |> List.map ((fun i -> sprintf "%i" i) >> char) |> Array.ofList |> cstring

solveB "03036732577212944063491565474664"
|> solution "16b.1" "84462026"
solveB "02935109699940807407585447034323"
|> solution "16b.3" "78725270"
solveB "03081770884921959731165446850517"
|> solution "16b.2" "53553731"
solveB initial
|> solution "16b" "42178738"