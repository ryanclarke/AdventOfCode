#load "utils/Base.fs"
open Aoc.Base

let program =
    inputFile "02" string
    |> Seq.exactlyOne
    |> ssplit [|','|]
    |> Array.map int

let intcodeComputer02a (input: int[]) (noun,verb) =
    let mutable memory = Array.copy input
    memory.[1] <- noun
    memory.[2] <- verb

    let mutable index = 0
    while memory.[index] < 99 do
        match memory.[index] with
        | 1 ->
            memory.[memory.[index+3]] <- (memory.[memory.[index+1]] + memory.[memory.[index+2]])
        | 2 ->
            memory.[memory.[index+3]] <- (memory.[memory.[index+1]] * memory.[memory.[index+2]])
        | _ -> failwith "Invalid Opcode"
        |> ignore
        index <- index + 4
    memory.[0]

intcodeComputer02a program (12,2)
|> solution "02a"

let intcodeComputer02b expected =
    square (seq {0 .. 99})
    |> Seq.pick (fun nv -> 
        let actual = intcodeComputer02a program nv
        if actual = expected
        then 
            let noun,verb = nv
            Some (100 * noun + verb)
        else
            None)

intcodeComputer02b 19690720
|> solution "02b"
