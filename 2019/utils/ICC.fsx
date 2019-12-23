module IntcodeComputer =
    type Control = Continue | Wait | Halt
    type Snapshot = {
        Memory: int64[]
        InstructionPointer: int
        RelativeBase: int
        Output: int64 list
        Halted: bool
    }

    let init program =
        let memory = Array.append program (Array.create 3000 0L)
        {Memory = memory; InstructionPointer = 0; RelativeBase = 0; Output = []; Halted = false}

    let execute (input:int64 list) (state: Snapshot) =
        let mutable memory = state.Memory
        let mutable instructionPointer = state.InstructionPointer
        let mutable relativeBase = state.RelativeBase
        let mutable output = []
        let mutable input = input // |> List.map int64

        let getMode i =
            let divisor = (pown 10 (i + 2))
            (int memory.[instructionPointer]) / divisor % 10

        let set (index:int) (value:int64) =
            match (getMode index) with
            | 0 -> int (memory.[instructionPointer + 1 + index])
            | 1 -> instructionPointer + 1 + index
            | 2 -> relativeBase + int (memory.[instructionPointer + 1 + index])
            | err -> failwithf "Invalid mode: %A" err
            |> (fun key -> memory.[key] <- value)

        let get (index:int) : int64 =
            match (getMode index) with
            | 0 -> int (memory.[instructionPointer + 1 + (int index)])
            | 1 -> instructionPointer + 1 + (int index)
            | 2 -> relativeBase + int (memory.[instructionPointer + 1 + (int index)])
            | err -> failwithf "Invalid mode: %A" err
            |> (fun key -> memory.[key])

        let debug currentOp x =
            // printfn "%04d %A ~~ %s" instructionPointer currentOp x
            ()

        let mutable control = Continue
        while (control = Continue) do
            let skip =
                //let currentOp = opcode (int memory.[instructionPointer])
                
                let opcode = 
                    (int memory.[instructionPointer]) % 100;

                match opcode with
                | 01 -> // Addition
                        // debug "ADDN" (sprintf "%A + %A => %A" (get 0) (get 1) (get 2))
                        set 2 ((get 0) + (get 1))
                        instructionPointer + 1 + 3
                | 02 -> // Multiplication
                        // debug currentOp (sprintf "%A * %A => %A" (get 0) (get 1) (get 2))
                        set 2 ((get 0) * (get 1))
                        instructionPointer + 1 + 3
                | 03 -> // Input
                        match input with
                        | [] ->
                            control <- Wait
                            instructionPointer
                        | inp::remainingInputs ->
                            // debug currentOp (sprintf "%A => %A" inp (get 0))
                            set 0 inp
                            input <- remainingInputs
                            instructionPointer + 1 + 1
                | 04 -> // Output
                        let out = get 0
                        // debug currentOp (sprintf "%A" out)
                        // dumps "===>" out
                        output <- out::output
                        instructionPointer + 1 + 1
                | 05 -> // Jump If True
                        let v = get
                        // debug currentOp (sprintf "%A %A" (v 0) (v 1))
                        match (v 0) <> 0L with
                        | true -> int (v 1)
                        | false -> instructionPointer + 1 + 2
                | 06 -> // Jump If False
                        let v = get
                        // debug currentOp (sprintf "%A %A" (v 0) (v 1))
                        match (v 0) = 0L with
                        | true -> int (v 1)
                        | false -> instructionPointer + 1 + 2
                | 07 -> // Less Than
                        let v = get
                        // debug currentOp (sprintf "%A < %A => %A" (v 0) (v 1) (v 2))
                        let value =
                            match (v 0) < (v 1) with
                            | true -> 1L
                            | false -> 0L
                        set 2 value
                        instructionPointer + 1 + 3
                | 08 -> // Equals
                        let v = get
                        // debug currentOp (sprintf "%A = %A => %A" (v 0) (v 1) (v 2))
                        let value =
                            match (v 0) = (v 1) with
                            | true -> 1L
                            | false -> 0L
                        set 2 value
                        instructionPointer + 1 + 3
                | 09 -> // Set Relative Base
                        let v = get
                        // debug currentOp (sprintf "%A => %A" relativeBase (v 0))
                        relativeBase <- relativeBase + (v 0 |> int)
                        instructionPointer + 1 + 1
                | 99 -> // Exit
                        // debug currentOp ""
                        control <- Halt
                        0
                | err -> failwithf "Invalid opcode: %A" err
            instructionPointer <- skip

        match control with
        | Halt -> {Memory = memory; InstructionPointer = instructionPointer; RelativeBase = relativeBase; Output = output; Halted = true}
        | Wait -> {Memory = memory; InstructionPointer = instructionPointer; RelativeBase = relativeBase; Output = output; Halted = false}
        | Continue -> failwith "Should be continuing"
    
    let run (input:int list) (memory: int64[]) =
        memory
        |> init
        |> execute (input |> List.map int64)
        |> (fun x -> x.Output)
