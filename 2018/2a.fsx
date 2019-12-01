System.IO.File.ReadLines("input/2.txt")
|> Seq.map (fun x ->
    x |> Seq.groupBy id |> Seq.map (snd >> Seq.length) |> fun g -> Seq.contains 2 g, Seq.contains 3 g)
|> fun x ->
    (x |> Seq.filter fst |> Seq.length) * (x |> Seq.filter snd |> Seq.length)
|> printfn "%A"