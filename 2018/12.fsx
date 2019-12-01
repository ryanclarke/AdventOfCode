let parseToBool c =
    match c with
    | '#' -> true
    | '.' -> false
    | _ -> failwith "Parsing error"

let parseToBools (input:string) =
    input
    |> Seq.map (fun c -> parseToBool c)
    |> List.ofSeq

let parseNote (input:string) =
    (input.[0..4] |> List.ofSeq, input.[9])

let input = System.IO.File.ReadLines "input/12.txt" |> List.ofSeq

let initialPots =
    input.[0].[15..]
    |> List.ofSeq
    |> fun x ->
        List.init 160 (fun _ -> '.') 
        |> List.append x
        |> List.append (List.init 10 (fun _ -> '.'))

let notes =
    input.[1..]
    |> List.filter (System.String.IsNullOrWhiteSpace >> not)
    |> List.map parseNote
    |> Map.ofSeq

let folder (pots:char list) gen =
    pots
    |> List.windowed 5
    |> List.map (fun w ->
        notes.[w]
        )
    |> fun x ->
        List.init 2 (fun _ -> '.') 
        |> List.append x
        |> List.append (List.init 2 (fun _ -> '.'))
 
[1..20]
|> List.fold folder (initialPots)
|> List.mapi (fun i p ->
    if parseToBool p
    then i-10
    else 0)
|> List.sum
|> printfn "20 generations: %d"
 
[1..159]
|> List.fold folder (initialPots)
|> List.mapi (fun i p ->
    if parseToBool p
    then (int64 i)-10L+(50000000000L-159L)
    else 0L)
|> List.sum
|> printfn "Fifty billion generations: %d"

