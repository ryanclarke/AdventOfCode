#load "utils/Base.fsx"
open Base

type Cell = {
    Dist: int
    Weigth: int
    Coord: int * int
    Done: bool
    Code: char
    Neighbors: (int * int) list
}

type Portal = {
    Name: string
    Coord: int * int
}

let run file start target =
    let maze =
        sprintf "input/%s.txt" file
        |> System.IO.File.ReadLines
        |> Seq.map (Seq.toArray)
        |> Array.ofSeq

    let width = maze.[0].Length-1
    let height = maze.Length-1
    let ys = [| 0 .. height |]
    let xs = [| 0 .. width |]

    let space = '.'
    let isSpace c = c = space
    let isPortal c = c >= 'A' && c <= 'Z'
    let isNowhere c = c = '#' || c = ' '
    let isPassage c = isSpace c || isPortal c

    let cardsFrom (y,x) = [
        if x > 0 then yield (y,x-1)
        if x < width then yield (y,x+1)
        if y > 0 then yield (y-1,x)
        if y < height then yield (y+1,x)
    ]
    let isGood c = (not c.Done) && isSpace c.Code
    let minDist (cells:Cell [] []) = cells |> Array.collect id |> Array.filter isGood |> Array.minBy (fun c -> c.Dist)
    // let neighborMap = ys |> Array.collect (fun y -> xs |> Array.map (fun x -> ((y,x),(cardsFrom (y,x))))) |> Map.ofArray

    let dumpScreen tick (minX,maxX,minY,maxY) (cells:Cell [] []) =
        [minY .. maxY] |> List.iter (fun y ->
            [minX .. maxX] |> List.map (fun x ->
                char cells.[y].[x].Code)
            |> dumpcs
        )
        printfn "======= #%A %A" tick (minX,maxX,minY,maxY)

    let dumpFullscreen = dumpScreen 0 (0,width,0,height)

    let distance start target =
        let mutable cells =
            ys |> Array.map (fun y -> xs |> Array.map (fun x ->
                let code = maze.[y].[x]
                {
                    Dist = System.Int32.MaxValue;
                    Weigth = if isPortal code then 0 else 1;
                    Coord = (y,x);
                    Done = not (isSpace code);
                    Code = maze.[y].[x];
                    Neighbors = cardsFrom (y,x)
                }))
        let cellOf (y,x) = cells.[y].[x]
        let setCell (y,x) c = cells.[y].[x] <- c
        let goodNeighbors c = c.Neighbors |> List.map cellOf |> List.filter (fun c -> isPassage c.Code)
        let createPortal middle =
            let ns = goodNeighbors middle
            if ns.Length = 2
            then
                let outer = ns |> List.find (fun c -> isPortal c.Code)
                let space = ns |> List.find (fun c -> isSpace c.Code)
                let name =
                    match ((outer.Coord),(space.Coord)) with
                    | (o,_),(s,_) | (_,o),(_,s) when o < s -> [outer.Code;middle.Code] |> cstring
                    | _ -> [middle.Code;outer.Code] |> cstring
                Some {Name = name; Coord = space.Coord}
            else None
        let portals =
            cells |> Array.collect id |> Array.toList |> List.filter (fun c -> isPortal c.Code) |> List.choose createPortal
        let portalMap =
            portals |> List.groupBy (fun p -> p.Name) |> List.collect (fun (_,ps) ->
                match ps with
                | [a;b] -> [(a.Coord,b.Coord);(b.Coord,a.Coord)]
                | [a] -> []
                | err -> failwithf "Invalid portal pair %A" err) |> Map.ofList
        let coordsForPortal (s:string) =
            portals |> List.filter (fun p -> p.Name = s) |> List.map (fun p -> p.Coord)
        let getNeighbors (c:Cell) =
            if portalMap.ContainsKey c.Coord then [portalMap.[c.Coord]] else []
            |> List.append c.Neighbors

        let startY,startX = (coordsForPortal start).Head
        let targetCoord = (coordsForPortal target).Head

        cells.[startY].[startX] <- {cells.[startY].[startX] with Dist = 0}

        let mutable minX = startX
        let mutable maxX = startX
        let mutable minY = startY
        let mutable maxY = startY
        // dump ([|[|cells.[startY].[startX]|]|] |> minDist allowedChars)

        let isAvailable (y,x) = cells.[y].[x] |> isGood
        let mutable targetDist = 0
        let mutable tick = 0

        let mutable currentCell = minDist cells
        while (targetDist = 0 && currentCell.Dist < 100000) do
            getNeighbors currentCell
            |> List.map cellOf
            |> List.iter (fun cell ->
                let y,x = cell.Coord
                let dist = currentCell.Weigth + currentCell.Dist
                if dist < cell.Dist
                then
                    if x < minX then minX <- x
                    if x > maxX then maxX <- x
                    if y < minY then minY <- y
                    if y > maxY then maxY <- y
                    setCell cell.Coord {cell with Dist = dist}
                
                if cell.Coord = targetCoord
                then targetDist <- dist
            )
            setCell currentCell.Coord {currentCell with Done = true}
        
            // dumpScreen tick (minX,maxX,minY,maxY) cells |> ignore

            let myY,myX = currentCell.Coord
            cells.[myY].[myX] <- {currentCell with Done = true}
            tick <- tick + 1
            currentCell <- minDist cells
        
        targetDist
    
    distance start target

// ===================================================================================================================
// 2019.20 A
// ===================================================================================================================

let solveA file = 
    run file "AA" "ZZ"

solveA "20test.1"
|> solution "20a.1" 58
solveA "20"
|> solution "20a" 544

// ===================================================================================================================
// 2019.20 B
// ===================================================================================================================

let solveB = 0

solveB
|> solution "20b" 0