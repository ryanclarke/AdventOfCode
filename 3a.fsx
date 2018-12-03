open System.Drawing
open System

type Claim = {
    Id: int
    Rect: Rectangle
    }

let mutable chart = Array.create 1000000 false;

let parseClaim (s:string) =
    s.Split([|' '; '#'; '@'; ','; 'x'; ':'|])
        |> Array.filter (String.IsNullOrWhiteSpace >> not)
        |> Array.map int
        |> fun x ->
            {
                Id = x.[0]
                Rect = new Rectangle(x.[1], x.[2], x.[3], x.[4])
            }

let intersects a b =
    if (a.Rect.IntersectsWith(b.Rect)) then
        let inter = Rectangle.Intersect (a.Rect, b.Rect)
        seq { inter.Top .. inter.Bottom-1 }
            |> Seq.iter (fun x ->
                seq { inter.Left .. inter.Right-1 }
                    |> Seq.iter (fun y ->
                        chart.[x*1000 + y] <- true))
        Some (inter)
    else
        None

let claims =
    System.IO.File.ReadLines("input/3.txt")
        |> Seq.map parseClaim
        |> Seq.toList

claims.[..(claims.Length-2)]
    |> List.mapi (fun i x -> claims.[i+1..] |> List.map (intersects x) |> List.choose id) 

chart |> Array.filter id |> Array.length |> printfn "%A"
