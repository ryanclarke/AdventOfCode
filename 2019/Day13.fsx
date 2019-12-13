open System.Collections.Generic
#load "utils/Base.fsx"
open Base

module IntcodeComputer =
    type Control = Continue | Wait | Halt
    type Snapshot = {
        Memory: Dictionary<int64,int64>
        InstructionPointer: int64
        RelativeBase: int64
        Output: int64 list
        Halted: bool
    }

    type ParameterMode = Position | Immediate | Relative

    type Opcode =
        | ADDN of ParameterMode[]
        | MULT of ParameterMode[]
        | INPT of ParameterMode[]
        | OUTP of ParameterMode[]
        | JIFT of ParameterMode[]
        | JIFF of ParameterMode[]
        | LESS of ParameterMode[]
        | EQAL of ParameterMode[]
        | RELB of ParameterMode[]
        | EXIT
    
    let rec parameterMode state code length =
        let newState =
            match (code % 10L) with
            | 0L -> Position
            | 1L -> Immediate
            | 2L -> Relative
            | m -> failwithf "Invalid ParameterMode: %A" m
            ::state
        match (newState.Length = length) with
        | false -> parameterMode newState (code / 10L) length;
        | true -> newState |> List.rev |> List.toArray

    let opcode code =
        let p = parameterMode [] (code / 100L)
        match (code % 100L) with
        | 01L -> ADDN (p 3)
        | 02L -> MULT (p 3)
        | 03L -> INPT (p 1)
        | 04L -> OUTP (p 1)
        | 05L -> JIFT (p 2)
        | 06L -> JIFF (p 2)
        | 07L -> LESS (p 3)
        | 08L -> EQAL (p 3)
        | 09L -> RELB (p 1)
        | 99L -> EXIT
        | op -> failwithf "Invalid Opcode: %A" op

    let toDictionary (map : Map<_, _>) : Dictionary<_, _> = Dictionary(map)

    let init program =
        {Memory = program |> Array.mapi (fun i a -> (int64 i),a) |> Map.ofArray |> toDictionary; InstructionPointer = 0L; RelativeBase = 0L; Output = []; Halted = false}

    let execute (state: Snapshot) (inputx:int64 list) =
        let mutable memory = state.Memory
        let mutable instructionPointer = state.InstructionPointer
        let mutable relativeBase = state.RelativeBase
        let mutable output = []
        let mutable input = inputx

        let set (modes:ParameterMode[]) index value =
            match modes.[index] with
            | Position -> memory.[instructionPointer + 1L + (int64 index)]
            | Immediate -> instructionPointer + 1L + (int64 index)
            | Relative -> relativeBase + memory.[instructionPointer + 1L + (int64 index)]
            |> (fun key ->
                if (memory.ContainsKey key)
                then memory.[key] <- value 
                else memory.Add (key,value))

        let get (modes:ParameterMode[]) index =
            match modes.[index] with
            | Position -> memory.[instructionPointer + 1L + (int64 index)]
            | Immediate -> instructionPointer + 1L + (int64 index)
            | Relative -> relativeBase + memory.[instructionPointer + 1L + (int64 index)]
            |> (fun key ->
                if not (memory.ContainsKey key)
                then (set modes index 0L |> ignore)
                memory.[key])

        let debug currentOp (modes:ParameterMode[]) x =
            // printfn "%04d %A %A ~~ %s" instructionPointer currentOp ([0 .. (modes.Length)] |> List.map (fun x -> memory.[instructionPointer + int64 x])) x
            ()

        let mutable control = Continue
        while (control = Continue) do
            let skip =
                let currentOp = opcode memory.[instructionPointer]
                match currentOp with
                | ADDN modes ->
                    let v = get modes
                    debug currentOp modes (sprintf "%A + %A => %A" (v 0) (v 1) (v 2))
                    set modes 2 ((v 0) + (v 1))
                    instructionPointer + 1L + (int64 modes.Length)
                | MULT modes ->
                    let v = get modes
                    debug currentOp modes (sprintf "%A * %A => %A" (v 0) (v 1) (v 2))
                    set modes 2 ((v 0) * (v 1))
                    instructionPointer + 1L + (int64 modes.Length)
                | INPT modes ->
                    match input with
                    | [] ->
                        control <- Wait
                        instructionPointer
                    | inp::remainingInputs ->
                        debug currentOp modes (sprintf "%A => %A" inp (get modes 0))
                        set modes 0 inp
                        input <- remainingInputs
                        instructionPointer + 1L + (int64 modes.Length)
                | OUTP modes -> 
                    let out = get modes 0
                    debug currentOp modes (sprintf "%A" out)
                    // dumps "===>" out
                    output <- out::output
                    instructionPointer + 1L + (int64 modes.Length)
                | JIFT modes -> 
                    let v = get modes
                    debug currentOp modes (sprintf "%A %A" (v 0) (v 1))
                    match (v 0) <> 0L with
                    | true -> (v 1)
                    | false -> instructionPointer + 1L + (int64 modes.Length)
                | JIFF modes -> 
                    let v = get modes
                    debug currentOp modes (sprintf "%A %A" (v 0) (v 1))
                    match (v 0) = 0L with
                    | true -> (v 1)
                    | false -> instructionPointer + 1L + (int64 modes.Length)
                | LESS modes -> 
                    let v = get modes
                    debug currentOp modes (sprintf "%A < %A => %A" (v 0) (v 1) (v 2))
                    let value =
                        match (v 0) < (v 1) with
                        | true -> 1L
                        | false -> 0L
                    set modes 2 value
                    instructionPointer + 1L + (int64 modes.Length)
                | EQAL modes -> 
                    let v = get modes
                    debug currentOp modes (sprintf "%A = %A => %A" (v 0) (v 1) (v 2))
                    let value =
                        match (v 0) = (v 1) with
                        | true -> 1L
                        | false -> 0L
                    set modes 2 value
                    instructionPointer + 1L + (int64 modes.Length)
                | RELB modes ->
                    let v = get modes
                    debug currentOp modes (sprintf "%A => %A" relativeBase (v 0))
                    relativeBase <- relativeBase + (v 0)
                    instructionPointer + 1L + (int64 modes.Length)
                | EXIT ->
                    debug currentOp [||] ""
                    control <- Halt
                    0L
            instructionPointer <- skip
        match control with
        | Halt -> {Memory = memory; InstructionPointer = instructionPointer; RelativeBase = relativeBase; Output = output; Halted = true}
        | Wait -> {Memory = memory; InstructionPointer = instructionPointer; RelativeBase = relativeBase; Output = output; Halted = false}
        | Continue -> failwith "Should be continuing"
    
    let run (memoryx: int64[]) (input:int64 list) =
        let state = init memoryx
        (execute state input).Output

let program =
    inputFile "13" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64


let rec toTiles tiles raw =
    match raw with
    | tileId::y::x::rest ->
        toTiles (((x,y),tileId)::tiles) rest
    | _ -> tiles

// ===================================================================================================================
// 2019.13 A
// ===================================================================================================================

let solveA program =
    let state = IntcodeComputer.init program
    (IntcodeComputer.execute state []).Output
    |> toTiles []
    |> List.map snd
    |> List.groupBy id
    |> Map.ofList
    |> fun x -> x.[2L].Length

solveA program
|> solution "13a" 333

// ===================================================================================================================
// 2019.13 B
// ===================================================================================================================
    
let dumpScreen tiles score =
    tiles |> Array.iter (fun row ->
        row |> Array.map (fun tileId ->        
            match tileId with
            | 0 -> ' ' // space
            | 1 -> '█' // wall
            | 2 -> '▪' // block
            | 3 -> '▬' // paddle
            | 4 -> '☺' // ball
            | err -> failwithf "Bad tileId: %A" err
        ) |> dumpcs
    )
    printfn "SCORE: %6i" score

let solveB program =
    let xs = [| 0 .. 43 |]
    let ys = [| 0 .. 20 |] 

    Array.set program 0 2L
    let mutable state = IntcodeComputer.init program
    let mutable tiles = ys |> Array.map (fun _ -> xs |> Array.map (fun _ -> 0))
    let mutable input = []
    let mutable score = 0

    tiles.[0].[0] <- 2

    let blocksRemain tiles = tiles |> Array.exists (fun xs -> xs |> Array.exists (fun tileId -> tileId = 2))
    let xOfPaddle tiles = (tiles |> Array.choose (fun xs -> xs |> Array.tryFindIndex (fun tileId -> tileId = 3))).[0]
    let xOfBall tiles = (tiles |> Array.choose (fun xs -> xs |> Array.tryFindIndex (fun tileId -> tileId = 4))).[0]

    while blocksRemain tiles do
        state <- IntcodeComputer.execute state input
        state.Output
        |> List.map int
        |> toTiles []
        |> List.iter (fun ((x,y),tileId) ->
            if x = -1
            then score <- tileId
            else tiles.[y].[x] <- tileId)

        input <-
            match (xOfBall tiles,xOfPaddle tiles) with
            | b,p when b > p -> [1L]
            | b,p when b < p -> [-1L]
            | _ -> [0L]

    dumpScreen tiles score
    score

solveB program
|> solution "13b" 16539

