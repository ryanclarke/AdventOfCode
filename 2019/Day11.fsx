open System.Collections.Generic
#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let program =
    inputFile "11" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

let black = 0
let white = 1

type Heading = N | E | W | S
type Turn = L | R

let emergencyHullPaintingRobot (program:int64[]) startingColor =
    let mutable state = IntcodeComputer.init program
    let mutable hull = new Dictionary<(int*int),int> ()
    let mutable heading = N
    let mutable location = (0,0)

    let setColor color =
        if (hull.ContainsKey location)
        then hull.[location] <- color
        else hull.Add (location, color)

    let getColor _ =
        if not (hull.ContainsKey location)
        then setColor black
        hull.[location]

    let move (lx,ly) (dx,dy) = (lx+dx,ly+dy)

    let toTurn i =
        match i with
        | 0 -> L
        | 1 -> R
        | err -> failwithf "Invalid turn: %A" err

    let go turn =
        let newHeading,changeLocation =
            match (heading,toTurn turn) with
            | N,R | S,L -> E,(0,1)
            | N,L | S,R -> W,(0,-1)
            | E,R | W,L -> S,(-1,0)
            | E,L | W,R -> N,(1,0)
        heading <- newHeading
        location <- move location changeLocation

    setColor startingColor

    while not state.Halted do
        state <- state |> IntcodeComputer.execute [getColor ()]
        match state.Output |> List.map int with
        | [turn;color] ->
            setColor color
            go turn
        | err -> failwithf "Invalid outputs: %A" err

    hull

// ===================================================================================================================
// 2019.11 A
// ===================================================================================================================

let solveA program =
    (emergencyHullPaintingRobot program black).Count

solveA program
|> solution "11a" 2339

// ===================================================================================================================
// 2019.11 B
// ===================================================================================================================

let solveB program =
    let hull = emergencyHullPaintingRobot program white
    let coords = hull |> List.ofSeq |> List.map (fun kvp -> kvp.Key)
    
    let maxX = coords |> List.map fst |> List.max
    let minX = coords |> List.map fst |> List.min
    let maxY = coords |> List.map snd |> List.max
    let minY = coords |> List.map snd |> List.min
    
    let xs = [ minX .. maxX ]
    let ys = [ minY .. maxY ] 

    xs |> List.rev |> List.iter (fun x ->
        ys |> List.map (fun y ->
            let color = if (hull.ContainsKey(x,y)) then hull.[(x,y)] else 0
            match color with
            | 0 -> ' '
            | 1 -> '█'
            | err -> failwithf "Bad color: %A" err)
        |> dumpcs)
    0

// PGUEPLPR
solveB program
|> solution "11b" 0