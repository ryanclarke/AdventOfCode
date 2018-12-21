// Idiomatic Python 3 solution curtesy of 
// https://www.reddit.com/r/adventofcode/comments/a7uk3f/2018_day_20_solutions/ec61vb0/

open System

let d =
    [('N',(0, -1));
     ('E',(1, 0));
     ('S',(0, 1));
     ('W',(-1, 0))]
    |> Map.ofList

let mutable positions = []
let mutable pos = (5000,5000)
let mutable m = Map.empty
let mutable prevPos = pos
let mutable dists = Map.empty
let mutable dist = 0

let add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

let input =
    System.IO.File.ReadAllLines("input/20.txt").[0].TrimStart('^').TrimEnd('$')

input
|> Seq.iter (fun c ->
    match c with
    | '(' -> 
        positions <- pos::positions
    | ')' ->
        pos <- positions.Head
        positions <- positions.Tail
    | '|' ->
        pos <- positions.Head
    | c -> 
        let diff = d.[c]
        pos <- add pos diff
        if m.ContainsKey pos
        then m <- m.Add(pos,(prevPos::m.[pos]))
        else m <- m.Add(pos,[prevPos])
        if dists.ContainsKey pos
        then dists <- dists.Add(pos,Math.Min(dists.[pos], dists.[prevPos]+1))
        else 
            if dists.ContainsKey prevPos
            then dists <- dists.Add(pos,dists.[prevPos]+1)
            else dists <- dists.Add(pos,1)
    prevPos <- pos
    )

printfn "%A" (dists |> Map.toSeq |> Seq.map snd |> Seq.max)