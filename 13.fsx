let input =
    System.IO.File.ReadLines "input/13.txt"
    |> Seq.map Seq.toArray
    |> Seq.toArray

let trackDiagram =
    input
    |> Array.map (fun row ->
        row |> Array.map (fun cell ->
            match cell with
            | '<' -> '-'
            | '>' -> '-'
            | '^' -> '|'
            | 'v' -> '|'
            | _ -> cell
            ))

type Train = {
    Coord: int*int
    Glyph: char
    LastFork: int
}

type TrainState = {
    Offset: int*int
    Opposite: char
    Curve: Map<char,char>
    Turns: char list
}

let dumpDiagram trains =
    let trainsMap =
        trains
        |> Array.map (fun t -> (t.Coord,t))
        |> Map.ofArray
    trackDiagram
    |> Array.mapi (fun y row ->
        row
        |> Array.mapi (fun x cell ->
            if trainsMap |> Map.containsKey (x,y)
            then trainsMap.[(x,y)].Glyph
            else cell))

let run _ =
    let mutable noCrash = true
    let mutable crashCoordinates = []
    let mutable trains =
        input
        |> Array.mapi (fun y row ->
            row
            |> Array.mapi (fun x cell ->
                if ['<';'>';'^';'v'] |> List.contains cell
                then Some {Coord = (x,y); Glyph = cell; LastFork = 0}
                else None
                )
            |> Array.choose id)
        |> Array.collect id

    let trainState = 
        [('<', {Offset = (-1,0); Opposite = '>'; Curve = Map.ofList ['\\', '^'; '/', 'v']; Turns = ['v';'<';'^']});
         ('^', {Offset = (0,-1); Opposite = 'v'; Curve = Map.ofList ['\\', '<'; '/', '>']; Turns = ['<';'^';'>']});
         ('>', {Offset = (1,0); Opposite = '<'; Curve = Map.ofList ['\\', 'v'; '/', '^']; Turns = ['^';'>';'v']});
         ('v', {Offset = (0,1); Opposite = '^'; Curve = Map.ofList ['\\', '>'; '/', '<']; Turns = ['>';'v';'<']})]
        |> Map.ofList

    let transform train =
        let x,y = train.Coord
        let ts = trainState.[train.Glyph]
        let dx,dy = ts.Offset
        let newCoord = (x+dx,y+dy)
        if trains |> Array.exists (fun t -> t.Coord = newCoord)
        then 
            noCrash <- false
            crashCoordinates <- newCoord::crashCoordinates
            {train with Coord = newCoord}
        else
            let newTrack = trackDiagram.[y+dy].[x+dx]
            if ts.Curve.ContainsKey newTrack
            then 
                {train with Coord = newCoord; Glyph = ts.Curve.[newTrack]}
            else
                if newTrack = '+'
                then 
                    {train with Coord = newCoord; Glyph = ts.Turns.[train.LastFork % 3]; LastFork = train.LastFork + 1}
                else {train with Coord = newCoord}

    while noCrash do
        trains |> Array.iteri (fun i t ->
            trains.[i] <- transform t)
    
    crashCoordinates |> List.last |> printfn "Coordinates of first crash: %A"

run ()