open System
open System.Diagnostics

let rand = new Random()

let rands = [| for _ in 1 .. 1000 -> int (rand.NextDouble() * 1000.0) |]

let arrayCollectMap size = 
    [|0 .. size|] |> Array.collect (fun y ->
        [|0 .. size|] |> Array.map (fun x -> (x,y)))
    |> Array.last

let arrayForInDo size =
    [| for y in 0 .. size do
        for x in 0 .. size ->
            (x,y)
    |]
    |> Array.last

let seqForInDo size =
    seq { for y in 0 .. size do
            for x in 0 .. size ->
                (x,y)
    }
    |> Seq.last

let test func =
    func 100 |> ignore
    let sw = Stopwatch.StartNew()
    rands |> Array.map func |> ignore
    printfn "%dms" sw.ElapsedMilliseconds

test arrayCollectMap
test arrayForInDo
test seqForInDo