#load "utils/Base.fsx"
open Base

module intcodeComputer =
    type ParameterMode = Position | Immediate

    type Opcode =
        | ADDN of ParameterMode[]
        | MULT of ParameterMode[]
        | INPT of ParameterMode[]
        | OUTP of ParameterMode[]
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
                match opcode memory.[instructionPointer] with
                | ADDN modes ->
                    let v = valueOf modes
                    // printfn "ADDN %A %A %A ~~ %d + %d => %d" instructionPointer modes memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1) memory.[(parameter 2)]
                    memory.[(parameter 2)] <- (v 0) + (v 1)
                    modes.Length
                | MULT modes ->
                    let v = valueOf modes
                    // printfn "MULT %A %A %A ~~ %d * %d => %d" instructionPointer modes memory.[instructionPointer..(instructionPointer+modes.Length)] (v 0) (v 1) memory.[(parameter 2)]
                    memory.[(parameter 2)] <- (v 0) * (v 1)
                    modes.Length
                | INPT modes ->
                    // printfn "INPT %A %A %A ~~ %A => %A" instructionPointer modes memory.[instructionPointer..(instructionPointer+modes.Length)] input.Head memory.[parameter 0]
                    memory.[parameter 0] <- input.Head
                    input <- input.Tail
                    modes.Length
                | OUTP modes -> 
                    let out = valueOf modes 0
                    // printfn "OUTP %A %A %A ~~ %A" instructionPointer modes memory.[instructionPointer..(instructionPointer+modes.Length)] out
                    dumps "===>" out
                    output <- out::output
                    modes.Length
                | EXIT ->
                    //printfn "EXIT %A %A" instructionPointer memory.[instructionPointer]
                    halt <- true
                    0
            instructionPointer <- instructionPointer + 1 + skip

        output.Head

let program =
    inputFile "05" string
    |> Seq.exactlyOne
    |> ssplit [|','|]
    |> Array.map int

intcodeComputer.run [1] program
|> solution "05a" 4511442

