let toLower = System.Char.ToLower

let reactsWith a b =
    toLower a = toLower b && a <> b

let polymerReactor (state:char list) (c:char) : char list =
    match state with
    | [] -> [c]
    | x::xs -> if (x |> reactsWith c) then xs else c::state

let run polymer =
    polymer
    |> Seq.fold polymerReactor []
    |> Seq.length

let initialPolymer =
    System.IO.File.ReadLines("input/5.txt") |> Seq.exactlyOne

let filterPolymers filter =
    initialPolymer |> Seq.filter (fun x -> toLower x <> filter)

let runWithFilters =
    seq { 'a' .. 'z' }
    |> Seq.map (filterPolymers >> run)
    |> Seq.min

printfn "Part 1: %A" <| run initialPolymer
printfn "Part 2: %A" <| runWithFilters