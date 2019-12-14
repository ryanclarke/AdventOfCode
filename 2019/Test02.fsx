#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let program =
    inputFile "02" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

// ===================================================================================================================
// 2019.02 A
// ===================================================================================================================

let nounVerbComputer noun verb (program: int64[]) =
    program.[1] <- int64 noun
    program.[2] <- int64 verb

    program
    |> IntcodeComputer.init
    |> IntcodeComputer.execute []
    |> (fun s -> s.Memory.[0] |> int)

nounVerbComputer 12 2 program
|> solution "02a" 3306701

// ===================================================================================================================
// 2019.02 B
// ===================================================================================================================

let findNounVerbFor expected =
    square (seq {0 .. 99})
    |> Seq.pick (fun (noun,verb) -> 
        if expected = nounVerbComputer  noun verb program
        then Some (100 * noun + verb)
        else None)

findNounVerbFor 19690720
|> solution "02b" 7621
