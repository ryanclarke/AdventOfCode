#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let program =
    inputFile "15" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

let N = 1
let S = 2
let E = 3
let W = 4

let Unknown = -1
let Wall = 0
let Space = 1
let O2Sys = 2

let cardsFrom (x,y) = [
    (x,y-1)
    (x,y+1)
    (x+1,y)
    (x-1,y)
]

type Cell = {
    Dist: int
    Path: int list
    Coord: int * int
    Done: bool
    Status: int
}

let dumpScreen tick (minX,maxX,minY,maxY) (cells:Cell [] []) =
    [minY .. maxY] |> List.iter (fun y ->
        [minX .. maxX] |> List.map (fun x ->
            match (cells.[x].[y]).Status with
            | -2 -> ['[';'#';']']
            | 0 -> ['█';'█';'█']
            | 1 -> (sprintf "%03i" (cells.[x].[y]).Path.Length) |> Seq.toList
            | 2 -> ['>';'X';'<']
            | _ -> [' ';' ';' ']
        )|> List.collect id |> dumpcs
    )
    printfn "======= #%A %A" tick (minX,maxX,minY,maxY)

let minDist (cells:Cell [] []) =
    cells |> Array.collect id |> Array.filter (fun c -> not (c.Done || c.Status = Wall)) |> Array.minBy (fun s -> s.Dist)

let createMap =
    let center = 25
    let xs = [| 0 .. (center*2) |]
    let ys = [| 0 .. (center*2) |]
    let map = xs |> Array.collect (fun x -> ys |> Array.map (fun y -> ((x,y),(cardsFrom (x,y))))) |> Map.ofArray
    let mutable cells = xs |> Array.map (fun x -> ys |> Array.map (fun y -> {Dist = System.Int32.MaxValue; Path = []; Coord = (x,y); Done = false; Status = -1}))
    let source = (center,center)
    cells.[center].[center] <- {cells.[center].[center] with Dist = 0; Status = -2}

    let isAvailable (x,y) = cells.[x].[y] |> fun c -> not (c.Done || c.Status = Wall)

    // let mutable state = IntcodeComputer.init program
    let run path = IntcodeComputer.run (path |> List.rev) program |> List.head |> int

    let mutable minX = center
    let mutable maxX = center
    let mutable minY = center
    let mutable maxY = center

    let mutable tick = 0

    while ((minDist cells).Dist < System.Int32.MaxValue) do
        tick <- tick + 1
        let currentCell = minDist cells
        //printfn "======= #%A %A" tick currentCell
        let myX,myY = currentCell.Coord
        let possibilities = map.[currentCell.Coord]

        possibilities
        |> List.iteri (fun i (x,y) ->
            if isAvailable (x,y)// |> dumprs (sprintf "coord %A,%A Available:" x y)
            then
                let cell = cells.[x].[y]
                let path = (i+1)::currentCell.Path
                if path.Length < cell.Dist
                then
                    if x < minX then minX <- x
                    if x > maxX then maxX <- x
                    if y < minY then minY <- y
                    if y > maxY then maxY <- y
                    let status = path |> List.map int64 |> run
                    cells.[x].[y] <- {cell with Dist = path.Length; Path = path; Status = status}
        )
        // dumpScreen tick (minX,maxX,minY,maxY) cells

        cells.[myX].[myY] <- {currentCell with Done = true}
    
    // dumpScreen tick (minX,maxX,minY,maxY) cells
    cells

let mutable locationOfO2Sys = createMap |> Array.collect id |> Array.tryFind (fun c -> c.Status = O2Sys) |> fun c -> c.Value

// ===================================================================================================================
// 2019.15 A
// ===================================================================================================================

let solveA =
    locationOfO2Sys.Path.Length

solveA
|> solution "15a" 272

// ===================================================================================================================
// 2019.15 B
// ===================================================================================================================

let solveB =
    let o2sysPath = locationOfO2Sys |> fun c -> c.Path |> List.rev
    let furthestCornerPath = createMap |> Array.collect id |> Array.map (fun c -> c.Path) |> Array.maxBy (fun p -> p.Length) |> List.rev
    
    let isCommonAtIndex i = if o2sysPath.[i] = furthestCornerPath.[i] then None else Some (i)
    let commonPathToOrigin = Seq.initInfinite isCommonAtIndex |> Seq.choose id |> Seq.head

    o2sysPath.Length + furthestCornerPath.Length - (commonPathToOrigin * 2)

solveB
|> solution "15b" 398