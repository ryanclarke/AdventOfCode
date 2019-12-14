#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

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
        let output = IntcodeComputer.run (currentPhase::input) (program |> Array.map int64) |> List.map int
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
        let output = IntcodeComputer.run (currentPhase::input) program |> List.map int
        runAmplifierB program output remainingPhases
    | [] -> input.Head

let runAmplifiersB program (phaseSettings:int[]) =
    let amplifierStates =
        phaseSettings
        |> Array.map (fun ps -> {            
            IntcodeComputer.Snapshot.Memory = program |> Array.copy;
            IntcodeComputer.Snapshot.InstructionPointer = 0;
            IntcodeComputer.Snapshot.RelativeBase = 0;
            IntcodeComputer.Snapshot.Output = [];
            IntcodeComputer.Snapshot.Halted = false})

    let inputs = phaseSettings |> Array.map (fun ps -> [ps])
    let addInputs index is = inputs.[index] <- List.append inputs.[index] is
    addInputs 0 [0]

    let mutable iter = 0
    let mutable halt = false
    while (not halt) do
        let nextIndex = (iter + 1) % 5
        let currIndex = iter % 5

        let inState = amplifierStates.[currIndex]
        match inState.Halted with
        | true -> halt <- true
        | false ->
            let outState = IntcodeComputer.execute inputs.[currIndex] inState
            amplifierStates.[currIndex] <- outState
            if iter < 4
            then addInputs nextIndex (outState.Output |> List.map int)
            else inputs.[nextIndex] <- (outState.Output |> List.map int)
            iter <- iter + 1

    (int amplifierStates.[4].Output.Head,phaseSettings)

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
            | 5 -> l |> List.toArray |> Some
            | _ -> None)

let solveB program =
    allPhaseSettingOrdersB ()
    |> Seq.map (runAmplifiersB (program |> Array.map int64))
    |> Seq.maxBy fst

solveB [|3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5|]
|> solution "07b.1" (139629729,[|9;8;7;6;5|])
solveB [|3;52;1001;52;-5;52;3;53;1;52;56;54;1007;54;5;55;1005;55;26;1001;54;-5;54;1105;1;12;1;53;54;53;1008;54;0;55;1001;55;1;55;2;53;55;53;4;53;1001;56;-1;56;1005;56;6;99;0;0;0;0;10|]
|> solution "07b.2" (18216,[|9;7;8;5;6|])
solveB program
|> solution "07b" (35993240,[|5;7;8;9;6|])