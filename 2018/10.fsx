let sw = System.Diagnostics.Stopwatch.StartNew()

type Light = {
    Position: int*int
    Velocity: int*int
} 

type State = {
    Lights: Light[]
    Iteration: int
}

let parse input =
    let regexPattern = "position=< ?(?<x>-?\d+),  ?(?<y>-?\d+)> velocity=< ?(?<dx>-?\d+),  ?(?<dy>-?\d+)>"
    let m = System.Text.RegularExpressions.Regex.Match(input, regexPattern)
    if (m.Success)
    then
        let iVal (key:string) = int m.Groups.[key].Value
        {Position = (iVal "x", iVal "y");
         Velocity = (iVal "dx", iVal "dy")}
    else failwith "Parsing Error"

let update state i =
    let updateLight light =
        {light with
            Position = 
                (fst light.Position + fst light.Velocity, 
                 snd light.Position + snd light.Velocity)}

    {Lights = state.Lights |> Array.map updateLight;
     Iteration = i}

let printIt state =
    let lights = state.Lights |> Array.map (fun x -> x.Position)
    let l = (lights |> Array.minBy fst) |> fst
    let t = (lights |> Array.minBy snd) |> snd
    let r = (lights |> Array.maxBy fst) |> fst
    let b = (lights |> Array.maxBy snd) |> snd
    let ons =
        lights |> Set.ofSeq
    [| t .. b |]
    |> Array.iter (fun y ->
        [| l .. r |]
        |> Array.map (fun x -> if ons.Contains (x,y) then '█' else ' ')
        |> System.String
        |> printfn "%s")
    printfn "On iteration %d after %dms" state.Iteration sw.ElapsedMilliseconds

let start =
    {Lights = 
        System.IO.File.ReadLines("input/10.txt")
        |> Array.ofSeq
        |> Array.map parse
     Iteration = 0}

[| 1 .. 10710 |]
|> Array.fold update start
|> printIt