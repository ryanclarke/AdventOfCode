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

    let run (inputx:int list) (memoryx: int[]) =
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

        let instruction modes func a b c =
           let v = valueOf modes
           memory.[(parameter c)] <- func (v a) (v b)
           true
        
        let mutable halt = false
        while (not halt) do
            let skip =
                let currentOp = opcode memory.[instructionPointer]
                match currentOp with
                | ADDN modes ->
                    let v = valueOf modes
                    // printfn "%04d %A %A ~~ %A + %A => %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1) memory.[(parameter 2)]
                    memory.[(parameter 2)] <- (v 0) + (v 1)
                    instructionPointer + 1 + modes.Length
                | MULT modes ->
                    let v = valueOf modes
                    // printfn "%04d %A %A ~~ %A * %A => %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1) memory.[(parameter 2)]
                    memory.[(parameter 2)] <- (v 0) * (v 1)
                    instructionPointer + 1 + modes.Length
                | INPT modes ->
                    // printfn "%04d %A %A ~~ %A => %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] input.Head memory.[parameter 0]
                    memory.[parameter 0] <- input.Head
                    input <- input.Tail
                    instructionPointer + 1 + modes.Length
                | OUTP modes -> 
                    let out = valueOf modes 0
                    // printfn "%04d %A %A ~~ %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] out
                    // dumps "===>" out
                    output <- out::output
                    instructionPointer + 1 + modes.Length
                | JIFT modes -> 
                    let v = valueOf modes
                    // printfn "%04d %A %A ~~ %A %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1)
                    match (v 0) <> 0 with
                    | true -> (v 1)
                    | false -> instructionPointer + 1 + modes.Length
                | JIFF modes -> 
                    let v = valueOf modes
                    // printfn "%04d %A %A ~~ %A %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1)
                    match (v 0) = 0 with
                    | true -> (v 1)
                    | false -> instructionPointer + 1 + modes.Length
                | LESS modes -> 
                    let v = valueOf modes
                    // printfn "%04d %A %A ~~ %A < %A => %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1) (v 2)
                    memory.[(parameter 2)] <-
                        match (v 0) < (v 1) with
                        | true -> 1
                        | false -> 0
                    instructionPointer + 1 + modes.Length
                | EQAL modes -> 
                    let v = valueOf modes
                    // printfn "%04d %A %A ~~ %A = %A => %A" instructionPointer currentOp memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1) (v 2)
                    memory.[(parameter 2)] <-
                        match (v 0) = (v 1) with
                        | true -> 1
                        | false -> 0
                    instructionPointer + 1 + modes.Length
                | EXIT ->
                    // printfn "%04d %A %A" instructionPointer currentOp memory.[instructionPointer]
                    halt <- true
                    0
            instructionPointer <- skip

        output

let program =
    inputFile "05" string
    |> Seq.exactlyOne
    |> ssplit [|','|]
    |> Array.map int

intcodeComputer.run [1] program
|> List.head |> solution "05a" 4511442

intcodeComputer.run [7] [|3;9;8;9;10;9;4;9;99;-1;8|]
|> List.head |> solution "05b.1a" 0
intcodeComputer.run [8] [|3;9;8;9;10;9;4;9;99;-1;8|]
|> List.head |> solution "05b.1b" 1

intcodeComputer.run [7] [|3;9;7;9;10;9;4;9;99;-1;8|]
|> List.head |> solution "05b.2a" 1
intcodeComputer.run [8] [|3;9;7;9;10;9;4;9;99;-1;8|]
|> List.head |> solution "05b.2b" 0

intcodeComputer.run [7] [|3;3;1108;-1;8;3;4;3;99|]
|> List.head |> solution "05b.3a" 0
intcodeComputer.run [8] [|3;3;1108;-1;8;3;4;3;99|]
|> List.head |> solution "05b.3b" 1

intcodeComputer.run [7] [|3;3;1107;-1;8;3;4;3;99|]
|> List.head |> solution "05b.4a" 1
intcodeComputer.run [8] [|3;3;1107;-1;8;3;4;3;99|]
|> List.head |> solution "05b.4b" 0

intcodeComputer.run [0] [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|]
|> List.head |> solution "05b.5a" 0
intcodeComputer.run [1] [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|]
|> List.head |> solution "05b.5b" 1

intcodeComputer.run [0] [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|]
|> List.head |> solution "05b.6a" 0
intcodeComputer.run [1] [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|]
|> List.head |> solution "05b.6b" 1

intcodeComputer.run [7] [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|]
|> List.head |> solution "05b.7a" 999
intcodeComputer.run [8] [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|]
|> List.head |> solution "05b.7b" 1000
intcodeComputer.run [9] [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|]
|> List.head |> solution "05b.7c" 1001

intcodeComputer.run [5] program
|> List.head |> solution "05b" 12648139

