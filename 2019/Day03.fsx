#load "utils/Base.fs"
open Aoc.Base

let input =
    inputFile "03" string
    |> Seq.toArray

type Line = {
    track: int
    start: int
    max: int
    min: int
    steps: int
}

type Segment =
    | H of Line
    | V of Line

let segmentize (x,y,steps) (vector:string) =
    let direction = vector.[0]
    let distance = int vector.[1..]
    let newSteps = steps + distance
    match direction with
    | 'U' -> (H {track = x; start = y; max = y + distance; min = y; steps = steps},(x, y + distance, newSteps))
    | 'D' -> (H {track = x; start = y; max = y; min = y - distance; steps = steps},(x, y - distance, newSteps))
    | 'R' -> (V {track = y; start = x; max = x + distance; min = x; steps = steps},(x + distance, y, newSteps))
    | 'L' -> (V {track = y; start = x; max = x; min = x - distance; steps = steps},(x - distance, y, newSteps))
    | _ -> failwith "Invalid direction"

let toSegments str =
    str
    |> ssplit [|','|]
    |> Array.mapFold segmentize (0,0,0)
    |> fst
    |> Array.toSeq

let between a b =
    a.max > b.track && b.track > a.min

let intersection (segment1:Segment) (segment2:Segment) =
    match segment1, segment2 with
    | H h, V v | V v, H h
        when between v h && between h v ->
            let distance = h.steps + v.steps + (abs (v.track - h.start)) + (abs (h.track - v.start))
            Some (h.track,v.track,distance)
    | _ -> None

let intersectionsWith (wire:seq<Segment>) (segment:Segment) =
    wire
    |> Seq.choose (intersection segment)
    |> Seq.where (fun (x,y,_) -> x <> 0 && y <> 0)
    |> Seq.toList

let solve toDistance (input:string[]) =
    let wire1,wire2 =
        Array.map toSegments input
        |> fun wires -> wires.[0],wires.[1]
    wire1
    |> Seq.collect (intersectionsWith wire2)
    |> Seq.map toDistance
    |> Seq.min

let manhattanDistance (x,y,_) = abs x + abs y

input
|> solve manhattanDistance |> solution "03a 1211"

let combinedSteps (_,_,s) = s

input
|> solve combinedSteps |> solution "03b 101386"
