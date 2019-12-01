let ids = System.IO.File.ReadLines("input/2.txt") |> List.ofSeq

let rec compare (a:string) (b:string) i diffIndex =
    match i >= a.Length with
    | true -> diffIndex
    | false ->
        match a.[i] = b.[i] with
        | true -> compare a b (i+1) diffIndex
        | false ->
            match diffIndex with
            | None -> compare a b (i+1) (Some i)
            | Some _ -> None

let compareIdsAtIndexes i1 i2 =
    compare ids.[i1] ids.[i2] 0 None

let rec permutateTillSuccess i1 i2 =
    let i1, i2 =
        match i2 < ids.Length with
        | true -> i1, i2
        | false -> i1+1, 0
    match (compareIdsAtIndexes i1 i2) with
    | None -> permutateTillSuccess i1 (i2+1)
    | Some i -> ids.[i1] |> fun s -> s.[0..(i-1)] + s.[(i+1)..(s.Length-1)]

permutateTillSuccess 0 0 |> printfn "%A"