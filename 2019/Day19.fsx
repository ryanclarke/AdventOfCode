#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let initial =
    inputFile "19" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

// ===================================================================================================================
// 2019.19 A
// ===================================================================================================================

let solveA i =
    square [ 0 .. (i-1) ]
    |> Seq.filter (fun (x,y) -> (IntcodeComputer.run [int64 x;int64 y] initial).Head = 1L)
    |> Seq.toList
    |> List.length

solveA 50
|> solution "19a" 144

// ===================================================================================================================
// 2019.19 B
// ===================================================================================================================

let dumpScreen i l =
    let map = l |> Map.ofList
    [ 0 .. (i-1) ] |> List.iter (fun y -> 
        [ 0 .. (i-1) ] |> List.map (fun x -> 
            if map.[(x,y)] then '#' else '.'
        ) |> dumpcs
    )
    l

let inBeam (x,y) = (IntcodeComputer.run [int64 x;int64 y] initial).Head = 1L

type State = {
    Finished: bool
    Y: int
    MinX: int
    MaxXs: Map<int,int>
}

let solveB santasShipSize =
    let santasShipSizeInclusive = santasShipSize - 1

    let dumpln sq =
        sq |> Seq.map (fun (_,b) -> if b then '#' else '.') |> cstring

    let startX = 1340
    let state = {Finished = false; Y = 1520; MinX = startX; MaxXs = [(0,0)] |> Map.ofList}
    let runLine s =
        let y = s.Y + 1
        let unfolder (x,hasBeenInBeam) =
            let nextX = x + 1
            match hasBeenInBeam,inBeam (x,y) with
            | true,false -> None
            | _,inB ->
                let t = (x,inB)
                let state = (nextX,hasBeenInBeam || inB)
                Some (t,state)
        let line = Seq.unfold unfolder (startX,false)
        let inTractorBeam =
            line
            |> Seq.filter snd
            |> Seq.map (fst)
            |> Seq.toList

        let min = inTractorBeam |> List.min
        let max = inTractorBeam |> List.max

        let y100 = y - santasShipSizeInclusive
        let isFin = s.MaxXs.ContainsKey (y100) && s.MaxXs.[y100] = (min + santasShipSizeInclusive)
        
        let maxX100 = if s.MaxXs.ContainsKey (y100) then s.MaxXs.[y100] else min
        // printfn "Y: %4i %4i | X: %4i %4i | %8i | %s" y y100 min maxX100 (min*10000 + y100) (dumpln line)

        Some ((isFin,min,y100),{Finished = isFin; Y = y; MinX = min; MaxXs = Map.add y max s.MaxXs})

    Seq.unfold runLine state
    |> Seq.find (fun (isFin,_,_) -> isFin)
    |> fun (_,x,y) -> x * 10000 + y

solveB 100
|> solution "19b" 13561537