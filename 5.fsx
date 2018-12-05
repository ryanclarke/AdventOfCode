open System

let reactsWith a b =
    Char.ToLower a = Char.ToLower b && a <> b

let polymerReactor (state:char list) (c:char) : char list =
    match state with
    | [] -> [c]
    | x :: tail ->
        match x |> reactsWith c with
        | true -> tail
        | false -> c :: state

let cancelAllPolarities polymer =
    polymer
    |> List.fold polymerReactor []
    |> List.length

let initialPolymer =
    ["abBAcfg"]
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