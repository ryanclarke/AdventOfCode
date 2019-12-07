#load "utils/Base.fsx"
open Base

module intcodeComputer =
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

    let run (memoryx: int[]) (inputx:int list) =
        let mutable memory = Array.copy memoryx
        let mutable input = inputx
        let mutable output = []
        let mutable instructionPointer = 0
        
        let parameter index =
            memory.[instructionPointer+1+index]

        let valueOf (modes:ParameterMode[]) index =
            match modes.[index] with
            | Position -> memory.[parameter index]
            | Immediate -> parameter index

        let debug currentOp (modes:ParameterMode[]) x =
            //printfn "%04d %A %A ~~ %s" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] x
            ()
        
        let mutable halt = false
        while (not halt) do
            let skip =
                let currentOp = opcode memory.[instructionPointer]
                match currentOp with
                | ADDN modes ->
                    let v = valueOf modes
                    debug currentOp modes (sprintf "%A + %A => %A" (v 0) (v 1) memory.[(parameter 2)])
                    memory.[(parameter 2)] <- (v 0) + (v 1)
                    instructionPointer + 1 + modes.Length
                | MULT modes ->
                    let v = valueOf modes
                    debug currentOp modes (sprintf "%A * %A => %A" (v 0) (v 1) memory.[(parameter 2)])
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
                    instructionPointer + 1 + modes.Length
                | JIFT modes -> 
                    let v = valueOf modes
                    debug currentOp modes (sprintf "%A %A" (v 0) (v 1))
                    match (v 0) <> 0 with
                    | true -> (v 1)
                    | false -> instructionPointer + 1 + modes.Length
                | JIFF modes -> 
                    let v = valueOf modes
                    debug currentOp modes (sprintf "%A %A" (v 0) (v 1))
                    match (v 0) = 0 with
                    | true -> (v 1)
                    | false -> instructionPointer + 1 + modes.Length
                | LESS modes -> 
                    let v = valueOf modes
                    debug currentOp modes (sprintf "%A < %A => %A" (v 0) (v 1) (v 2))
                    memory.[(parameter 2)] <-
                        match (v 0) < (v 1) with
                        | true -> 1
                        | false -> 0
                    instructionPointer + 1 + modes.Length
                | EQAL modes -> 
                    let v = valueOf modes
                    debug currentOp modes (sprintf "%A = %A => %A" (v 0) (v 1) (v 2))
                    memory.[(parameter 2)] <-
                        match (v 0) = (v 1) with
                        | true -> 1
                        | false -> 0
                    instructionPointer + 1 + modes.Length
                | EXIT ->
                    debug currentOp [||] ""
                    halt <- true
                    0
            instructionPointer <- skip
        output

let rec runAmplifier program input (phaseSettings:int list) =
    match phaseSettings with
    | currentPhase::remainingPhases -> 
        let output = intcodeComputer.run program (currentPhase::input)
        runAmplifier program output remainingPhases
    | [] -> input.Head

let runAmplifiers program phaseSettings =
    (runAmplifier program [0] phaseSettings),phaseSettings

let allPhaseSettingOrders _ =
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

let solve program =
    allPhaseSettingOrders ()
    |> Seq.map (runAmplifiers program)
    |> Seq.maxBy fst

let program =
    inputFile "07" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int

solve [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|]
|> solution "07a.1" (43210,[4;3;2;1;0])
solve [|3;23;3;24;1002;24;10;24;1002;23;-1;23;101;5;23;23;1;24;23;23;4;23;99;0;0|]
|> solution "07a.2" (54321,[0;1;2;3;4])
solve [|3;31;3;32;1002;32;10;32;1001;31;-2;31;1007;31;0;33;1002;33;7;33;1;33;31;31;1;32;31;31;4;31;99;0;0;0|]
|> solution "07a.3" (65210,[1;0;4;3;2])
solve program
|> solution "07a" (368584,[2;4;0;1;3])