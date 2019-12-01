open System.Drawing
open System

type Claim = { Id: int; Rect: Rectangle }

let parseClaim (s:string) =
    s.Split([|' '; '#'; '@'; ','; 'x'; ':'|])
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map int
    |> fun x -> { Id = x.[0]; Rect = new Rectangle(x.[1], x.[2], x.[3], x.[4]) }

let intersects a (set:Set<int*int>) b =
    if (a.Rect.IntersectsWith(b.Rect)) then
        let rect = Rectangle.Intersect (a.Rect, b.Rect)
        seq { rect.Top .. rect.Bottom-1 }
        |> Seq.fold (fun (set:Set<int*int>) x ->
            seq { rect.Left .. rect.Right-1 }
            |> Seq.fold (fun (set:Set<int*int>) y ->
                set.Add (x, y)) set) set
    else
        set

System.IO.File.ReadLines("input/3.txt")
|> Seq.map parseClaim
|> Seq.toList
|> fun claims ->
    claims.[..(claims.Length-2)]
    |> List.mapi (fun i c -> claims.[i+1..] |> List.fold (intersects c) Set.empty)
    |> List.reduce Set.union
    |> Set.count
    |> printfn "%A"
