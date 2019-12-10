open System.Collections.Generic
#load "utils/Base.fsx"
open Base

module IntcodeComputer =
    type Control = Continue | Suspend | Halt
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

    let execute (state: Snapshot) (inputx:int64 list) =
        let mutable memory = state.Memory
        let mutable input = inputx
        let mutable output = state.Output |> List.toArray |> Array.copy |> Array.toList
        let mutable instructionPointer = state.InstructionPointer
        let mutable relativeBase = state.RelativeBase

        let set (modes:ParameterMode[]) index value =
            match modes.[index] with
            | Position -> memory.[instructionPointer + 1L + (int64 index)]
            | Immediate -> instructionPointer + 1L + (int64 index)
            | Relative -> relativeBase + memory.[instructionPointer + 1L + (int64 index)]
            |> (fun key ->
                if (memory.ContainsKey key)
                then memory.[key] <- value 
                else memory.Add (key,value))
        
        // let parameter index =
        //     memory.[instructionPointer + 1L + (int64 index)]

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
            //printfn "%04d %A %A ~~ %s" instructionPointer currentOp ([0 .. (modes.Length)] |> List.map (fun x -> memory.[instructionPointer + int64 x])) x
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
                    debug currentOp modes (sprintf "%A => %A" input.Head (get modes 0))
                    set modes  0 input.Head
                    input <- input.Tail
                    instructionPointer + 1L + (int64 modes.Length)
                | OUTP modes -> 
                    let out = get modes 0
                    debug currentOp modes (sprintf "%A" out)
                    // dumps "===>" out
                    output <- out::output
                    // control <- Suspend
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
        | Suspend -> {Memory = memory; InstructionPointer = instructionPointer; RelativeBase = relativeBase; Output = output; Halted = false}
        | Continue -> failwith "Should be continuing"
    
    let run (memoryx: int64[]) (input:int64 list) =
        let state = {Memory = memoryx |> Array.mapi (fun i a -> (int64 i),a) |> Map.ofArray |> toDictionary; InstructionPointer = 0L; RelativeBase = 0L; Output = []; Halted = false}
        execute state input
        |> (fun state -> state.Output)

let program =
    inputFile "09" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

// ===================================================================================================================
// 2019.09 A
// ===================================================================================================================

let solveA program =
    IntcodeComputer.run program [1L] |> List.rev

solveA [|109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L|]
|> solution "09a.1" [109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L]
solveA [|1102L;34915192L;34915192L;7L;4L;7L;99L;0L|]
|> solution "09a.2" [1219070632396864L]
solveA [|104L;1125899906842624L;99L|]
|> solution "09a.3" [1125899906842624L]
solveA program
|> solution "09a" [2377080455L]

// ===================================================================================================================
// 2019.09 B
// ===================================================================================================================


let solveB program =
    IntcodeComputer.run program [2L] |> List.rev

solveB program
|> solution "09b" [74917L]