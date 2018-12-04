open System.Drawing
open System

type Claim = { Id: int; Rect: Rectangle }

let parseClaim (s:string) =
    s.Split([|' '; '#'; '@'; ','; 'x'; ':'|])
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map int
    |> fun x -> { Id = x.[0]; Rect = new Rectangle(x.[1], x.[2], x.[3], x.[4]) }

let onlyOverlapsSelf claims claim =
    claims
    |> List.filter (fun a -> a.Rect.IntersectsWith(claim.Rect))
    |> fun x ->
        match (x.Length = 1) with
        | true -> Some(x.Head.Id)
        | false -> None

System.IO.File.ReadLines("input/3.txt")
|> Seq.map parseClaim
|> Seq.toList
|> fun claims ->
    claims
    |> List.choose (onlyOverlapsSelf claims)
    |> printfn "%A"
