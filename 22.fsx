let depth = 11394
let target = (7,701)
let tx,ty = target

let mutable erosionLevels = Array.zeroCreate ((tx+1)*(ty+1))
let indexOf (x,y) = x + y*(tx+1)
let erosionLevel geologicIndex = (geologicIndex + depth) % 20183

let calc coord =
    match coord with
    | 0,0 -> erosionLevels.[indexOf coord] <- erosionLevel 0
    | x,y when x=tx && y=ty -> erosionLevels.[indexOf coord] <- erosionLevel 0
    | x,0 -> erosionLevels.[indexOf coord] <- erosionLevel (x*16807)
    | 0,y -> erosionLevels.[indexOf coord] <- erosionLevel (y*48271)
    | x,y ->
        let px = erosionLevels.[indexOf (x-1,y)]
        let py = erosionLevels.[indexOf (x,y-1)]
        let e = px*py
        erosionLevels.[indexOf coord] <- erosionLevel e

[| 0 .. tx |] |> Array.collect (fun x ->
    [| 0 .. ty |] |> Array.map (fun y -> (x,y)))
|> Array.iter calc

erosionLevels
|> Array.map (fun a -> a % 3)
|> fun x -> printfn "Part 1: %d" (Array.sum x) //; x
// |> Array.map (fun a -> [|'.';'=';'|'|].[(a % 3)])
// |> Array.chunkBySize (ty+1)
// |> Array.iter (fun x -> x |> System.String |> printfn "%s")

