open System

let reactsWith a b =
    Char.ToLower a = Char.ToLower b && Char.IsLower a <> Char.IsLower b

let polymerFolder (state:char list) (c:char) =
    match state |> List.isEmpty with
    | true -> [c]
    | false ->
        state
        |> List.head
        |> reactsWith c
        |> fun x ->
            match x with
            | true -> state |> List.skip 1
            | false -> c :: state

let cancelAllPolarities polymer =
    polymer
    |> List.fold polymerFolder List.empty
    |> List.length

let initialPolymer =
    System.IO.File.ReadLines("input/5.txt")
    |> Seq.head
    |> Seq.toList

let run filter =
    initialPolymer
    |> List.filter (fun x -> Char.ToLower x <> filter)
    |> cancelAllPolarities

let runWithFilters =
    seq { 'a' .. 'z' }
    |> Seq.map run
    |> Seq.min

run ' ' |> printfn "Part 1: %A"
runWithFilters |> printfn "Part 2: %A"