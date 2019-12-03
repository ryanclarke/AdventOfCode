#load "utils/Base.fs"
open Aoc.Base

let input =
    inputFile "03" string
    |> Seq.toArray

type Segment =
    | H of (int*int*int)
    | V of (int*int*int)

let vectorize (x,y) (vector:string) =
    let direction = vector.[0]
    let distance = int vector.[1..]
    match direction with
    | 'U' -> (H (x,y + distance,y),(x, y + distance))
    | 'D' -> (H (x,y,y - distance),(x, y - distance))
    | 'R' -> (V (y,x + distance,x),(x + distance, y))
    | 'L' -> (V (y,x,x - distance),(x - distance, y))
    | _ -> failwith "Invalid direction"

let toSegments str =
    str
    |> ssplit [|','|]
    |> Array.mapFold vectorize (0,0)
    |> fst
    |> Array.toSeq

let crossing (segment1:Segment) (segment2:Segment) =
    match segment1, segment2 with
    | H h, V v | V v, H h ->
        match h,v with
        | (x,ymax,ymin),(y,xmax,xmin) when xmax >= x && x >= xmin && ymax >= y && y >= ymin -> Some (abs x + abs y)
        | _ -> None
    | _ -> None

let closest (wire2:seq<Segment>) (segment:Segment) =
    let crosses =
        wire2
        |> Seq.choose (crossing segment)
        |> Seq.where (fun x -> x > 0)
        |> Seq.toList

    match crosses with
    | [] -> None
    | x -> List.min x |> Some

let solve (input:string[]) =
    let wire1,wire2 =
        Array.map toSegments input
        |> fun wires -> wires.[0],wires.[1]
    wire1
    |> Seq.choose (closest wire2)
    |> Seq.min

solve input
|> solution "03a"
