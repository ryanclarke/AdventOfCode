#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let initial =
    inputFile "23" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

let solve =
    let size = 50
    let qs = [| 0 .. (size-1) |] |> Array.map (fun i -> [int64 i])
    let nics = [| 0 .. (size-1) |] |> Array.map (fun _ -> IntcodeComputer.init initial)
    let mutable nat = []
    let mutable firstNatY = 0L
    let mutable lastNatY = 0L
    let mutable repeatedNatY = false

    let mutable emptyCount = 0
    let getNextInputFor na =
        if na = 0 && List.isEmpty qs.[na] && emptyCount >= 49
        then
            emptyCount <- 0
            repeatedNatY <- lastNatY = nat.[1]
            lastNatY <- nat.[1]
            qs.[0] <- nat
            nat <- []
        match qs.[na] with
        | [] ->
            emptyCount <- emptyCount + 1
            [-1L]
        | q::rest ->
            emptyCount <- 0
            qs.[na] <- rest
            [q]

    let mutable loop = 0
    while not repeatedNatY do
        let na = loop % size
        nics.[na] <- IntcodeComputer.execute (getNextInputFor na) nics.[na]
        nics.[na].Output |> List.rev |> List.chunkBySize 3 |> List.iter (fun out ->
            match out with
            | [255L; x; y] ->
                if firstNatY = 0L then firstNatY <- y                
                nat <- [x; y]
            | [dest; x; y] -> qs.[int dest] <- List.append qs.[int dest] [x; y]
            | err -> failwithf "Invalid output: %A" err)
        
        loop <- loop + 1

    int firstNatY,int lastNatY

solve |> fst
|> solution "23a" 19040

solve |> snd
|> solution "23b" 11041