#load "utils/Base.fsx"
open Base

type Technique =
    | NewStack
    | Cut of int
    | Increment of int

let parse instruction =
    let values = instruction |> ssplit " " |> Array.toList |> List.rev |> List.take 2
    match values with
    | [n;"cut"] -> Cut (int n)
    | [n;"increment"] -> Increment (int n)
    | ["stack";"new"] -> NewStack
    | err -> failwithf "Invalid instruction: %A" err

let instructions =
    inputFile "22" string
    |> Seq.map parse

let newDeck = Seq.init 10007 id |> List.ofSeq

let shuffle deck instruction =
    match instruction with
    | NewStack -> deck |> List.rev
    | Cut i ->
        if i > 0
        then List.append deck.[i .. (deck.Length-1)] deck.[0 .. (i-1)]
        else List.append deck.[(deck.Length+i) .. (deck.Length-1)] deck.[0 .. (deck.Length+i-1)]
    | Increment incr ->
        deck
        |> List.mapi (fun idx card -> (((idx * incr) % deck.Length), card))
        |> List.sortBy fst
        |> List.map snd

// ===================================================================================================================
// 2019.22 A
// ===================================================================================================================

let solveA instructions =
    instructions
    |> Seq.fold shuffle newDeck
    |> Seq.toArray
    |> Array.findIndex (fun i -> i = 2019)
    |> dumpr

solveA instructions
|> solution "22a" 4775

// ===================================================================================================================
// 2019.22 B
// ===================================================================================================================

let solveB _ = 0

solveB instructions
|> solution "22b" 0