#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let initial =
    inputFile "23" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

// ===================================================================================================================
// 2019.23 A
// ===================================================================================================================

let solveA _ =
    let size = 50
    let qs = [| 0 .. (size-1) |] |> Array.map (fun i -> [int64 i])
    let nics = [| 0 .. (size-1) |] |> Array.map (fun _ -> IntcodeComputer.init initial)

    let getNextInputFor na =
        match qs.[na] with
        | [] -> [-1L]
        | q::rest ->
            qs.[na] <- rest
            [q]

    let mutable loop = 0
    let mutable result = None 
    while result.IsNone do
        let na = loop % size
        nics.[na] <- IntcodeComputer.execute (getNextInputFor na) nics.[na]
        // if nics.[na].Output <> [] then dumps (sprintf "%2i" na) (nics.[na].Output |> List.rev)
        nics.[na].Output |> List.rev |> List.chunkBySize 3 |> List.iter (fun out ->
            match out with
            | [255L; _; y] -> result <- Some y
            | [dest; x; y] -> qs.[int dest] <- List.append qs.[int dest] [x; y]
            | err -> failwithf "Invalid output: %A" err)
        
        // if loop % 1000 = 10 then (qs |> Array.filter (List.isEmpty >> not) |> Array.iteri (fun i x -> dumps (sprintf "q %2i" i) x))

        loop <- loop + 1

    int result.Value

solveA initial
|> solution "23a" 19040

// ===================================================================================================================
// 2019.23 B
// ===================================================================================================================

let solveB _ = 0

solveB initial
|> solution "23b" 0