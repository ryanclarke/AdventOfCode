#load "Utils.fsx"
open Utils

let (|Coordinate|_|) input =
    let m = System.Text.RegularExpressions.Regex.Match(input, "(?<x>\d+), (?<y>\d+)")
    if (m.Success)
    then Some (int m.Groups.["x"].Value, int m.Groups.["y"].Value)
    else None

let parseCoordinate str = 
    match str with
    | Coordinate c -> c
    | _ -> failwith "FAIL"

let coordinates =
    System.IO.File.ReadLines("input/6.txt") |> Seq.map parseCoordinate

let minX = coordinates |> Seq.minBy fst |> fst
let maxX = coordinates |> Seq.maxBy fst |> fst
let minY = coordinates |> Seq.minBy snd |> snd
let maxY = coordinates |> Seq.maxBy snd |> snd
let locations =
    seq { minX .. maxX } |> Seq.collect (fun x ->
        seq { minY .. maxY } |> Seq.map (fun y -> (x, y)))

let manhattanDistanceFrom loc coord =
    ((loc, coord), (abs (fst loc - fst coord) + abs (snd loc - snd coord)))

let locationCoordinates =
    coordinates |> Seq.collect (fun x ->
        locations |> Seq.map (manhattanDistanceFrom x))

let findClosestCoordinate lc =
    lc
    |> Seq.groupBy snd
    |> Seq.minBy fst
    |> snd
    |> fun x -> if Seq.length x > 1 then None else Seq.exactlyOne x |> Some

let locationClosestCoordinates =
    locationCoordinates
    |> Seq.groupBy (fst >> snd)
    |> Seq.map (snd >> findClosestCoordinate)
    |> Seq.choose id
    
    
    //  (fun x ->
    //     snd x
    //     |> Seq.groupBy snd
    //     |> Seq.minBy fst
    //     |> snd 
    //     |> fun y ->
    //         if y |> Seq.length > 1 
    //         then None
    //         else Seq.exactlyOne y |> Some)

let edgeCoordinates =
    locationClosestCoordinates
    |> Seq.map fst
    |> Seq.filter (fun x ->
        let loc, _ = x in
            fst loc = minX
         || snd loc = minY
         || fst loc = maxX
         || snd loc = maxY
        )
    |> Seq.map snd
    |> Set.ofSeq

locationClosestCoordinates
|> Seq.map fst
|> dumpr
|> Seq.filter (snd >> edgeCoordinates.Contains >> not)
|> Seq.countBy snd
|> Seq.map snd
|> Seq.max
|> printfn "Part 1: %A"

// locations
// |> Seq.map (fun x -> coordinates |> Seq.sumBy (fun y -> manhattanDistanceFrom x y |> fst))
// |> Seq.filter (fun x -> x < 10000)
// |> Seq.length
// |> printfn "Part 2: %A"
