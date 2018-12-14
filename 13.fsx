open System
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
    Id: int
    Coord: int*int
    Glyph: char
    LastFork: int
    Crashed: bool
    Moves: int
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
    let mutable crashCoordinates = []
    let mutable trains =
        input
        |> Array.mapi (fun y row ->
            row
            |> Array.mapi (fun x cell ->
                if ['<';'>';'^';'v'] |> List.contains cell
                then Some {Id = y*1000+x; Coord = (x,y); Glyph = cell; LastFork = 0; Crashed = false; Moves = 0}
                else None)
            |> Array.choose id)
        |> Array.collect id

    let activeTrains _ = trains |> Array.filter (fun t -> not t.Crashed)

    let trainState = 
        [('<', {Offset = (-1,0); Opposite = '>'; Curve = Map.ofList ['\\', '^'; '/', 'v']; Turns = ['v';'<';'^']});
         ('^', {Offset = (0,-1); Opposite = 'v'; Curve = Map.ofList ['\\', '<'; '/', '>']; Turns = ['<';'^';'>']});
         ('>', {Offset = (1,0); Opposite = '<'; Curve = Map.ofList ['\\', 'v'; '/', '^']; Turns = ['^';'>';'v']});
         ('v', {Offset = (0,1); Opposite = '^'; Curve = Map.ofList ['\\', '>'; '/', '<']; Turns = ['>';'v';'<']})]
        |> Map.ofList

    let transform trainO =
        let train = {trainO with Moves = trainO.Moves+1}
        let x,y = train.Coord
        let ts = trainState.[train.Glyph]
        let dx,dy = ts.Offset
        let newCoord = (x+dx,y+dy)
        if activeTrains() |> Array.exists (fun t -> t.Coord = newCoord)
        then 
            crashCoordinates <- newCoord::crashCoordinates
            {train with Coord = newCoord; Crashed = true}
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

    let setT old next =
        trains.[trains |> Array.findIndex (fun x -> old = x)] <- next
    
    while activeTrains() |> Array.length > 1 do
        trains |> Array.sortInPlaceBy (fun t -> t.Coord)
        trains
        |> Array.iter (fun train ->
            if train.Crashed
            then ()
            else
                let newT = transform train
                setT train newT
                if newT.Crashed
                then
                    activeTrains()
                    |> Array.filter (fun a ->
                        a.Coord = newT.Coord && a <> newT && (not a.Crashed))
                    |> Array.iter (fun a ->
                            setT a {a with Crashed = true}))
        
    
    crashCoordinates |> List.last |> printfn "Coordinates of first crash: %A"
    activeTrains() |> Array.exactlyOne |> fun t -> printfn "Coordinate of last train: %A" t.Coord

run ()