open System.Collections.Generic
#load "utils/Base.fsx"
open Base

type Xyz = {x:int; y:int; z:int}
type Moon = {p:Xyz; v:Xyz}

let xyz x y z = {x=x; y=y; z=z}
let initMoon x y z = {p=xyz x y z; v=xyz 0 0 0}
let dumpMoon m = printfn "pos=<x=%3i, y=%3i, z=%3i>, vel=<x=%3i, y=%3i, z=%3i>" m.p.x m.p.y m.p.z m.v.x m.v.y m.v.z

let inital = [ initMoon 1 2 -9; initMoon -1 -9 -4; initMoon 17 6 8; initMoon 12 4 2; ]
let example1 = [ initMoon -1 0 2; initMoon 2 -10 -7; initMoon 4 -8 8; initMoon 3 5 1; ]
let example2 = [ initMoon -8 -10 0; initMoon 5 5 10; initMoon 2 -7 3; initMoon 9 -8 -3; ]

let veloctiyChange i = 
    match i with
    | i when i > 0 -> 1
    | i when i < 0 -> -1
    | _ -> 0


// ===================================================================================================================
// 2019.12 A
// ===================================================================================================================

let solveA count moons =
    let sum a b = {x=a.x + b.x; y=a.y + b.y; z=a.z + b.z;}
    let gravity (a:Xyz) (b:Xyz) =
        let x = b.x - a.x |> veloctiyChange
        let y = b.y - a.y |> veloctiyChange
        let z = b.z - a.z |> veloctiyChange
        xyz x y z
    let calculateGravityForMoon a b = {a with v = gravity a.p b.p |> sum a.v}
    let calculateGravity moons moon = moons |> List.fold calculateGravityForMoon moon
    let updatePosition moon = {moon with p = sum moon.p moon.v}
    let performStep moons _ = moons |> List.map (calculateGravity moons >> updatePosition)
    let energy xyz = (abs xyz.x) + (abs xyz.y) + (abs xyz.z)
    let calculateEnergy moon = (energy moon.p) * (energy moon.v)

    [ 1 .. count ]
    |> List.fold performStep moons
    |> List.sumBy calculateEnergy

solveA 1000 inital
|> solution "12a" 7471

// ===================================================================================================================
// 2019.12 B
// ===================================================================================================================

type MoonAxis = int * int
type Axis = MoonAxis list
type Constelation = Axis list

let solveB moons =
    let (constellation:Constelation) = [
        moons |> List.map (fun m -> m.p.x,m.v.x);
        moons |> List.map (fun m -> m.p.y,m.v.y);
        moons |> List.map (fun m -> m.p.z,m.v.z);
    ]

    let gravity moonA moonB = (fst moonB) - (fst moonA) |> veloctiyChange
    
    let unfolder (i:int,stop:bool,state:Axis,original:Axis) =
        state |> List.map (fun moonAxis ->
            state
            |> List.sumBy (gravity moonAxis)
            |> fun newVelocity -> (fst moonAxis + snd moonAxis + newVelocity,snd moonAxis + newVelocity)
        ) |> fun a -> if stop then None else Some ((i+1,a),(i+1,(original = a && i > 10),a,original))

    constellation |> List.map (fun axis -> Seq.unfold unfolder (0,false,axis,axis) |> Seq.last |> fst)

solveB inital
|> solution "12b" [186028; 167624; 193052]