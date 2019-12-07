#load "utils/Base.fsx"
open Base

module IntcodeComputer =
    type Control = Continue | Suspend | Halt
    type Snapshot = {
        Memory: int[]
        InstructionPointer: int
        Output: int list
        Halted: bool
    }

    type ParameterMode = Position | Immediate

    type Opcode =
        | ADDN of ParameterMode[]
        | MULT of ParameterMode[]
        | INPT of ParameterMode[]
        | OUTP of ParameterMode[]
        | JIFT of ParameterMode[]
        | JIFF of ParameterMode[]
        | LESS of ParameterMode[]
        | EQAL of ParameterMode[]
        | EXIT
    
    let rec parameterMode state code length =
        let newState =
            match (code % 10) with
            | 0 -> Position
            | 1 -> Immediate
            | m -> failwithf "Invalid ParameterMode: %A" m
            ::state
        match (newState.Length = length) with
        | false -> parameterMode newState (code / 10) length;
        | true -> newState |> List.rev |> List.toArray

    let opcode code =
        let p = parameterMode [] (code / 100)
        match (code % 100) with
        | 01 -> ADDN (p 3)
        | 02 -> MULT (p 3)
        | 03 -> INPT (p 1)
        | 04 -> OUTP (p 1)
        | 05 -> JIFT (p 2)
        | 06 -> JIFF (p 2)
        | 07 -> LESS (p 3)
        | 08 -> EQAL (p 3)
        | 99 -> EXIT
        | op -> failwithf "Invalid Opcode: %A" op
    
    let execute (state: Snapshot) (inputx:int list) =
        let mutable memory = Array.copy state.Memory
        let mutable input = inputx
        let mutable output = state.Output |> List.toArray |> Array.copy |> Array.toList
        let mutable instructionPointer = state.InstructionPointer
        
        let parameter index =
            memory.[instructionPointer+1+index]

        let valueOf (modes:ParameterMode[]) index =
            match modes.[index] with
            | Position -> memory.[parameter index]
            | Immediate -> parameter index

        let debug currentOp (modes:ParameterMode[]) x =
            // printfn "%04d %A %A ~~ %s" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] x
            ()

        let mutable control = Continue
        while (control = Continue) do
            let skip =
                let currentOp = opcode memory.[instructionPointer]
                match currentOp with
                | ADDN modes ->
                    let v = valueOf modes
                    // debug currentOp modes (sprintf "%A + %A => %A" (v 0) (v 1) memory.[(parameter 2)])
                    memory.[(parameter 2)] <- (v 0) + (v 1)
                    instructionPointer + 1 + modes.Length
                | MULT modes ->
                    let v = valueOf modes
                    // debug currentOp modes (sprintf "%A * %A => %A" (v 0) (v 1) memory.[(parameter 2)])
                    memory.[(parameter 2)] <- (v 0) * (v 1)
                    instructionPointer + 1 + modes.Length
                | INPT modes ->
                    debug currentOp modes (sprintf "%A => %A" input.Head memory.[parameter 0])
                    memory.[parameter 0] <- input.Head
                    input <- input.Tail
                    instructionPointer + 1 + modes.Length
                | OUTP modes -> 
                    let out = valueOf modes 0
                    debug currentOp modes (sprintf "%A" out)
                    // dumps "===>" out
                    output <- out::output
                    control <- Suspend
                    instructionPointer + 1 + modes.Length
                | JIFT modes -> 
                    let v = valueOf modes
                    // debug currentOp modes (sprintf "%A %A" (v 0) (v 1))
                    match (v 0) <> 0 with
                    | true -> (v 1)
                    | false -> instructionPointer + 1 + modes.Length
                | JIFF modes -> 
                    let v = valueOf modes
                    // debug currentOp modes (sprintf "%A %A" (v 0) (v 1))
                    match (v 0) = 0 with
                    | true -> (v 1)
                    | false -> instructionPointer + 1 + modes.Length
                | LESS modes -> 
                    let v = valueOf modes
                    // debug currentOp modes (sprintf "%A < %A => %A" (v 0) (v 1) (v 2))
                    memory.[(parameter 2)] <-
                        match (v 0) < (v 1) with
                        | true -> 1
                        | false -> 0
                    instructionPointer + 1 + modes.Length
                | EQAL modes -> 
                    let v = valueOf modes
                    // debug currentOp modes (sprintf "%A = %A => %A" (v 0) (v 1) (v 2))
                    memory.[(parameter 2)] <-
                        match (v 0) = (v 1) with
                        | true -> 1
                        | false -> 0
                    instructionPointer + 1 + modes.Length
                | EXIT ->
                    debug currentOp [||] ""
                    control <- Halt
                    0
            instructionPointer <- skip
        match control with
        | Halt -> {Memory = memory; InstructionPointer = instructionPointer; Output = output; Halted = true}
        | Suspend -> {Memory = memory; InstructionPointer = instructionPointer; Output = output; Halted = false}
        | Continue -> failwith "Should be continuing"
    
    let run (memoryx: int[]) (input:int list) =
        let state = {Memory = memoryx; InstructionPointer = 0; Output = []; Halted = false}
        execute state input
        |> (fun state -> state.Output)

let program =
    inputFile "07" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int

// ===================================================================================================================
// 2019.07 A
// ===================================================================================================================

let rec runAmplifierA program input (phaseSettings:int list) =
    match phaseSettings with
    | currentPhase::remainingPhases -> 
        let output = IntcodeComputer.run program (currentPhase::input)
        runAmplifierA program output remainingPhases
    | [] -> input.Head

let runAmplifiersA program phaseSettings =
    (runAmplifierA program [0] phaseSettings),phaseSettings

let allPhaseSettingOrdersA _ =
    seq { 01234 .. 43210 }
    |> Seq.choose (fun d ->
        let l = [d/10000;(d/1000)%10;(d/100)%10;(d/10)%10;d%10]
        l
        |> List.filter (fun x -> x < 5)
        |> List.distinct
        |> List.length
        |> fun x -> 
            match x with
            | 5 -> l |> Some
            | _ -> None)

let solveA program =
    allPhaseSettingOrdersA ()
    |> Seq.map (runAmplifiersA program)
    |> Seq.maxBy fst

solveA [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|]
|> solution "07a.1" (43210,[4;3;2;1;0])
solveA [|3;23;3;24;1002;24;10;24;1002;23;-1;23;101;5;23;23;1;24;23;23;4;23;99;0;0|]
|> solution "07a.2" (54321,[0;1;2;3;4])
solveA [|3;31;3;32;1002;32;10;32;1001;31;-2;31;1007;31;0;33;1002;33;7;33;1;33;31;31;1;32;31;31;4;31;99;0;0;0|]
|> solution "07a.3" (65210,[1;0;4;3;2])
solveA program
|> solution "07a" (368584,[2;4;0;1;3])

// ===================================================================================================================
// 2019.07 B
// ===================================================================================================================

let rec runAmplifierB program input (phaseSettings:int list) =
    match phaseSettings with
    | currentPhase::remainingPhases -> 
        let output = IntcodeComputer.run program (currentPhase::input)
        runAmplifierB program output remainingPhases
    | [] -> input.Head

let runAmplifiersB program (phaseSettings:int list) =
    let amplifierStates =
        phaseSettings
        |> List.map (fun ps -> {            
            IntcodeComputer.Snapshot.Memory = program;
            IntcodeComputer.Snapshot.InstructionPointer = 0;
            IntcodeComputer.Snapshot.Output = [];
            IntcodeComputer.Snapshot.Halted = false})
        |> List.toArray
    
    let inputs = 
        phaseSettings
        |> List.map (fun ps -> [ps])
        |> List.toArray

    let addInputs index is =
        inputs.[index] <- List.append inputs.[index] is
    
    addInputs 0 [0]

    let mutable iter = 0
    let mutable halt = false
    while (not halt) do
        let nextIndex = (iter + 1) % 5
        let currIndex = (iter + 0) % 5
        let prevIndex = (iter + 4) % 5

        let inState = amplifierStates.[currIndex]
        match inState.Halted with
        | true -> halt <- true
        | false ->
            let outState = IntcodeComputer.execute inState inputs.[currIndex]
            amplifierStates.[currIndex] <- outState
            if iter < 4
            then addInputs nextIndex outState.Output
            else inputs.[nextIndex] <- outState.Output
            iter <- iter + 1
    
    // dump (phaseSettings,(amplifierStates |> Array.map (fun a -> a.Output.Head)))
    (amplifierStates.[4].Output.Head,phaseSettings)

let allPhaseSettingOrdersB _ =
    seq { 56789 .. 98765 }
    |> Seq.choose (fun d ->
        let l = [d/10000;(d/1000)%10;(d/100)%10;(d/10)%10;d%10]
        l
        |> List.filter (fun x -> x >= 5)
        |> List.distinct
        |> List.length
        |> fun x -> 
            match x with
            | 5 -> l |> Some
            | _ -> None)

let solveB program =
    allPhaseSettingOrdersB ()
    |> Seq.map (runAmplifiersB program)
    |> Seq.maxBy fst

solveB [|3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5|]
|> solution "07b.1" (139629729,[9;8;7;6;5])
solveB [|3;52;1001;52;-5;52;3;53;1;52;56;54;1007;54;5;55;1005;55;26;1001;54;-5;54;1105;1;12;1;53;54;53;1008;54;0;55;1001;55;1;55;2;53;55;53;4;53;1001;56;-1;56;1005;56;6;99;0;0;0;0;10|]
|> solution "07b.2" (18216,[9;7;8;5;6])
solveB program
|> solution "07b" (35993240,[5;7;8;9;6])