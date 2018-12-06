let (|Coordinate|_|) input =
    let m = System.Text.RegularExpressions.Regex.Match(input, "(?<x>\d+), (?<y>\d+)")
    if (m.Success)
    then Some (int m.Groups.["x"].Value, int m.Groups.["y"].Value)
    else None

let parseCoordinate str = 
    match str with
    | Coordinate c -> c
    | _ -> failwith "FAIL"

let locations =
    seq { 0 .. 399 } |> Seq.collect (fun x ->
        seq { 0 .. 399 } |> Seq.map (fun y -> (x, y)))

let coordinates =
    System.IO.File.ReadLines("input/6.txt") |> Seq.map parseCoordinate

let manhattanDistanceFrom loc coord =
    (abs (fst loc - fst coord) + abs (snd loc - snd coord), (loc, coord))

let findClosestCoordinate (location: int*int) =
    coordinates
    |> Seq.map (manhattanDistanceFrom location)
    |> Seq.groupBy fst
    |> Seq.minBy fst
    |> snd
    |> fun x -> if Seq.length x > 1 then None else Seq.exactlyOne x |> snd |> Some

let locationCoordinates =
    locations
    |> Seq.map findClosestCoordinate
    |> Seq.choose id

let findEdgeCoordinates =
    locationCoordinates
    |> Seq.filter (fun x ->
        let loc, _ = x in
            fst loc = 0
         || snd loc = 0
         || fst loc = 399
         || snd loc = 399
        )
    |> Seq.groupBy snd
    |> Seq.map fst
    |> Set.ofSeq

locationCoordinates
|> Seq.filter (snd >> findEdgeCoordinates.Contains >> not)
|> Seq.groupBy snd
|> Seq.maxBy (snd >> Seq.length)
|> snd
|> Seq.length
|> printfn "Part 1: %A"

locations
|> Seq.map (fun x -> coordinates |> Seq.sumBy (fun y -> manhattanDistanceFrom x y |> fst))
|> Seq.filter (fun x -> x < 10000)
|> Seq.length
|> printfn "Part 2: %A"
