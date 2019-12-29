#load "utils/Base.fsx"
open Base

type Coord = int * int * int

type Cell = {
    Dist: int
    Weigth: int
    Path: string list
    Coord: Coord
    Done: bool
    Code: char
    Neighbors: Coord list
}

type Portal = {
    Name: string
    Coord: Coord
}

let run levels file start target =
    let maze =
        sprintf "input/%s.txt" file
        |> System.IO.File.ReadLines
        |> Seq.map (Seq.toArray)
        |> Array.ofSeq

    let width = maze.[0].Length-1
    let height = maze.Length-1
    dump (width,height)
    let zs = [| 0 .. levels |]
    let ys = [| 0 .. height |]
    let xs = [| 0 .. width |]

    let space = '.'
    let isSpace c = c = space
    let isPortal c = c >= 'A' && c <= 'Z'
    let isNowhere c = c = '#' || c = ' '
    let isPassage c = isSpace c || isPortal c

    let cardsFrom (z,y,x) = [
        if x > 0 then yield (z,y,x-1)
        if x < width then yield (z,y,x+1)
        if y > 0 then yield (z,y-1,x)
        if y < height then yield (z,y+1,x)
    ]
    let isGood c = (not c.Done) && isSpace c.Code
    let minDist (cells:Cell [] [] []) = cells |> Array.collect id |> Array.collect id |> Array.filter isGood |> Array.minBy (fun c -> c.Dist)
    // let neighborMap = ys |> Array.collect (fun y -> xs |> Array.map (fun x -> ((y,x),(cardsFrom (y,x))))) |> Map.ofArray

    let dumpScreen tick (minX,maxX,minY,maxY) (cells:Cell [] [] []) =
        [minY .. maxY] |> List.iter (fun y ->
            [minX .. maxX] |> List.map (fun x ->
                char cells.[0].[y].[x].Code)
            |> dumpcs
        )
        printfn "======= #%A %A" tick (minX,maxX,minY,maxY)

    let dumpFullscreen = dumpScreen 0 (0,width,0,height)

    let distance start target =
        let mutable cells =
            zs |> Array.map (fun z -> ys |> Array.map (fun y -> xs |> Array.map (fun x ->
                let code = maze.[y].[x]
                {
                    Dist = System.Int32.MaxValue;
                    Weigth = if isPortal code then 0 else 1;
                    Path = [];
                    Coord = (z,y,x);
                    Done = not (isSpace code);
                    Code = code;
                    Neighbors = cardsFrom (z,y,x)
                })))
        let cellOf (z,y,x) = cells.[z].[y].[x]
        let setCell (z,y,x) c = cells.[z].[y].[x] <- c
        let goodNeighbors c = c.Neighbors |> List.map cellOf |> List.filter (fun c -> isPassage c.Code)
        let createPortal middle =
            let ns = goodNeighbors middle
            if ns.Length = 2
            then
                let outer = ns |> List.find (fun c -> isPortal c.Code)
                let space = ns |> List.find (fun c -> isSpace c.Code)
                let name =
                    match ((outer.Coord),(space.Coord)) with
                    | (_,o,_),(_,s,_) | (_,_,o),(_,_,s) when o < s -> [outer.Code;middle.Code] |> cstring
                    | _ -> [middle.Code;outer.Code] |> cstring
                match name with
                | "AA" | "ZZ" ->
                    let depth,_,_ = space.Coord
                    if depth = 0
                    then Some {Name = name; Coord = space.Coord}
                    else None
                | _ -> Some {Name = name; Coord = space.Coord}
            else None
        let portals =
            cells |> Array.collect id |> Array.collect id |> Array.toList |> List.filter (fun c -> isPortal c.Code) |> List.choose createPortal
        let portalMap =
            portals |> List.groupBy (fun p -> 
                let z,y,x = p.Coord
                p.Name,z)
            |> List.collect (fun (_,ps) ->
                match ps with
                | [a;b] ->
                    let az,ay,ax = a.Coord
                    let bz,by,bx = b.Coord
                    let newaz,newbz =
                        let achange = if ay = 2 || ay = (height-2) || ax = 2 || ax = (width-2) then 1 else -1
                        let bchange = achange * -1
                        if levels = 0
                        then 0,0
                        else az + achange,bz + bchange
                    [
                        if Array.contains newaz zs then yield (b.Coord,(newaz,ay,ax));
                        if Array.contains newbz zs then yield (a.Coord,(newbz,by,bx));
                    ]
                | [a] -> []
                | err -> failwithf "Invalid portal pair %A" err)
            |> Map.ofList
        let coordsForPortal (s:string) =
            portals |> List.filter (fun p -> p.Name = s) |> List.map (fun p -> p.Coord)
        let getNeighbors (c:Cell) =
            if portalMap.ContainsKey c.Coord then [portalMap.[c.Coord]] else []
            |> List.append c.Neighbors

        let startZ,startY,startX = (coordsForPortal start).Head
        let targetCoord = (coordsForPortal target).Head

        cells.[startZ].[startY].[startX] <- {cells.[startZ].[startY].[startX] with Dist = 0}

        let mutable minX = startX
        let mutable maxX = startX
        let mutable minY = startY
        let mutable maxY = startY

        let mutable targetDist = 0
        let mutable tick = 0

        let mutable currentCell = minDist cells
        dumptss "Starting"
        while (targetDist = 0 && currentCell.Dist < 100000) do
            let myZ,myY,myX = currentCell.Coord
            getNeighbors currentCell
            |> List.map cellOf
            |> List.iter (fun cell ->
                let z,y,x = cell.Coord
                let dist = currentCell.Weigth + currentCell.Dist
                if dist < cell.Dist
                then
                    if x < minX then minX <- x
                    if x > maxX then maxX <- x
                    if y < minY then minY <- y
                    if y > maxY then maxY <- y
                    let newPath =
                        if portalMap.ContainsKey cell.Coord
                        then (portals |> List.find (fun p -> p.Coord = cell.Coord) |> fun p -> sprintf "%s%2i" p.Name (p.Coord |> fun (z,_,_) -> z))::currentCell.Path
                        else currentCell.Path
                    setCell cell.Coord {cell with Dist = dist; Path = newPath}
                
                if cell.Coord = targetCoord
                then targetDist <- dist
            )
            setCell currentCell.Coord {currentCell with Done = true}
        
            //dumpScreen tick (minX,maxX,minY,maxY) cells |> ignore

            cells.[myZ].[myY].[myX] <- {currentCell with Done = true}
            tick <- tick + 1
            if tick % 1000 = 0 then dumptso (currentCell.Dist,currentCell.Path)
            if targetDist = 0 then currentCell <- minDist cells
        
        dump (currentCell.Dist,currentCell.Path)
        targetDist
    
    distance start target

// ===================================================================================================================
// 2019.20 A
// ===================================================================================================================

let solveA file = 
    run 0 file "AA" "ZZ"

solveA "20test.1"
|> solution "20a.1" 58
solveA "20"
|> solution "20a" 544

// ===================================================================================================================
// 2019.20 B
// ===================================================================================================================

let solveB file =
    run 40 file "AA" "ZZ"

solveB "20test.2"
|> solution "20b.1" 396
solveB "20"
|> solution "20b" 0