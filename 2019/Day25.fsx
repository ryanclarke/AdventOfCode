#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let initial =
    inputFile "25" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

let outputText (s:IntcodeComputer.Snapshot) =
    s.Output |> List.rev |> List.collect (fun i -> if i > 256L then string i |> List.ofSeq else [char i]) |> cstring

let dumpOutput (s:IntcodeComputer.Snapshot) =
    s |> outputText |> printfn "%s"

// ===================================================================================================================
// 2019.25 A
// ===================================================================================================================

let solveA initial =
    let commands = [
        "west";
        "take fixed point";
        "north";
        "take sand";
        "south";
        "east";
        "east";
        "north";
        "north";
        "north";
        "take coin";
        "south";
        "south";
        "west";
        "north";
        "take spool of cat6";
        "north";
        "west";
        "north";
    ]

    let mutable snapshot =
        initial
        |> IntcodeComputer.init
        |> IntcodeComputer.execute []

    commands
    |> List.iter (fun command ->
        let input = List.append (command |> List.ofSeq |> List.map int64) [10L]
        snapshot <- IntcodeComputer.execute input snapshot
        // dumpOutput snapshot
        )
    outputText snapshot
    |> ssplit "\n"
    |> Array.last
    |> ssplit " "
    |> Array.map (System.Int64.TryParse)
    |> Array.find fst
    |> snd

solveA initial
|> solution "25a" 2181046280L

// ===================================================================================================================
// 2019.25 B
// ===================================================================================================================

let solveB _ = 0

solveB initial
|> solution "25b" 0