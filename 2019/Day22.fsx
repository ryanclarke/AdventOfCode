open System.Numerics
#load "utils/Base.fsx"
open Base

// Many thanks to Reddit user mcpower_ for help with the solution to part B
// https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnkaju/

type Technique =
    | NewStack
    | Cut of bigint
    | Increment of bigint

let parse instruction =
    let values = instruction |> ssplit " " |> Array.toList |> List.rev |> List.take 2
    match values with
    | [n;"cut"] -> Cut (bigint (int n))
    | [n;"increment"] -> Increment (bigint (int n))
    | ["stack";"new"] -> NewStack
    | err -> failwithf "Invalid instruction: %A" err

let instructions =
    inputFile "22" string
    |> Seq.map parse

let compactPass (size:bigint) =
    let squish i = (i % size + size) % size
    let addTransform (offset,increment) instruction =
        let o,i =
            match instruction with
            | NewStack -> 
                let newIncrement = -1I * increment
                (offset + newIncrement,newIncrement)
            | Cut n -> ((offset + increment * n),increment)
            | Increment n -> (offset,increment * BigInteger.ModPow(n, size - 2I, size))
        (squish o, squish i)
    
    let offset,increment = 
        instructions
        |> Seq.fold addTransform (0I,1I)
    
    let valueAt = fun (i:int) (offset,increment) -> squish (offset + increment * (bigint i))
    (valueAt,(offset,increment))

// ===================================================================================================================
// 2019.22 A
// ===================================================================================================================

let solveA size =
    let valueAt,transform = compactPass size

    Seq.init (int size) id |> Seq.find (fun i -> transform |> valueAt i = 2019I)

solveA 10007I
|> solution "22a" 4775

// ===================================================================================================================
// 2019.22 B
// ===================================================================================================================

let solveB size iterations =
    let valueAt,(offset,increment) = compactPass size

    let endIncrement = BigInteger.ModPow(increment, iterations, size)
    let endOffset = offset * (1I - BigInteger.ModPow(increment, iterations, size)) * BigInteger.ModPow(1I - increment, size - 2I, size)

    (endOffset,endIncrement) |> valueAt 2020

solveB 119315717514047I 101741582076661I
|> solution "22b" 37889219674304I