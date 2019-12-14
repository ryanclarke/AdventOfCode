#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let program =
    inputFile "13" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

let rec toTiles tiles raw =
    match raw with
    | tileId::y::x::rest ->
        toTiles (((x,y),tileId)::tiles) rest
    | _ -> tiles

// ===================================================================================================================
// 2019.13 A
// ===================================================================================================================

let solveA program =
    (IntcodeComputer.init program |> IntcodeComputer.execute []).Output
    |> List.map int
    |> toTiles []
    |> List.map snd
    |> List.groupBy id
    |> Map.ofList
    |> fun x -> x.[2].Length

solveA program
|> solution "13a" 333

// ===================================================================================================================
// 2019.13 B
// ===================================================================================================================
    
let dumpScreen tiles score =
    tiles |> Array.iter (fun row ->
        row |> Array.map (fun tileId ->        
            match tileId with
            | 0 -> ' ' // space
            | 1 -> '█' // wall
            | 2 -> '▪' // block
            | 3 -> '▬' // paddle
            | 4 -> '☺' // ball
            | err -> failwithf "Bad tileId: %A" err
        ) |> dumpcs
    )
    printfn "SCORE: %6i" score

let solveB program =
    let xs = [| 0 .. 43 |]
    let ys = [| 0 .. 20 |] 

    Array.set program 0 2L
    let mutable state = IntcodeComputer.init program
    let mutable tiles = ys |> Array.map (fun _ -> xs |> Array.map (fun _ -> 0))
    let mutable input = []
    let mutable score = 0

    tiles.[0].[0] <- 2

    let blocksRemain tiles = tiles |> Array.exists (fun xs -> xs |> Array.exists (fun tileId -> tileId = 2))
    let xOfPaddle tiles = (tiles |> Array.choose (fun xs -> xs |> Array.tryFindIndex (fun tileId -> tileId = 3))).[0]
    let xOfBall tiles = (tiles |> Array.choose (fun xs -> xs |> Array.tryFindIndex (fun tileId -> tileId = 4))).[0]

    while blocksRemain tiles do
        state <- IntcodeComputer.execute input state
        state.Output
        |> List.map int
        |> toTiles []
        |> List.iter (fun ((x,y),tileId) ->
            if x = -1
            then score <- tileId
            else tiles.[y].[x] <- tileId)

        input <-
            match (xOfBall tiles,xOfPaddle tiles) with
            | b,p when b > p -> [1]
            | b,p when b < p -> [-1]
            | _ -> [0]

    // dumpScreen tiles score
    score

solveB program
|> solution "13b" 16539

