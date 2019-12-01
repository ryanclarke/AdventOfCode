let sw = System.Diagnostics.Stopwatch.StartNew()

let mutable reg = [|0; 0; 0; 0; 0; 0|]
let mutable ip = 0

let opcodes = 
    let toInt b = if b then 1 else 0

    [("addr", fun (a,b) -> reg.[a] + reg.[b]);
     ("addi", fun (a,b) -> reg.[a] + b);
     ("mulr", fun (a,b) -> reg.[a] * reg.[b]);
     ("muli", fun (a,b) -> reg.[a] * b);
     ("banr", fun (a,b) -> reg.[a] &&& reg.[b]);
     ("bani", fun (a,b) -> reg.[a] &&& b);
     ("borr", fun (a,b) -> reg.[a] ||| reg.[b]);
     ("bori", fun (a,b) -> reg.[a] ||| b);
     ("setr", fun (a,_) -> reg.[a]);
     ("seti", fun (a,_) -> a);
     ("gtir", fun (a,b) -> a > reg.[b] |> toInt);
     ("gtri", fun (a,b) -> reg.[a] > b |> toInt);
     ("gtrr", fun (a,b) -> reg.[a] > reg.[b] |> toInt);
     ("eqir", fun (a,b) -> a = reg.[b] |> toInt);
     ("eqri", fun (a,b) -> reg.[a] = b |> toInt);
     ("eqrr", fun (a,b) -> reg.[a] = reg.[b] |> toInt)]
    |> Map.ofList
    
let execInstruction opcode (a,b,c) =
    reg.[c] <- opcodes.[opcode] (a,b)


let parse input =
    let pattern = "(?<opcode>.+) (?<ints>\d+ \d+ \d+)"
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    let op = m.Groups.["opcode"].Value
    let i = m.Groups.["ints"].Value.Split(' ') |> Array.map int |> fun x -> (x.[0],x.[1],x.[2])
    (op,i)

let input = 
    System.IO.File.ReadLines("input/19.txt")
    |> Seq.toList

let ipReg =
    input
    |> List.take 1
    |> List.exactlyOne
    |> fun x -> x.Split(' ').[1] |> int

let instructions =
    input
    |> List.skip 1
    |> List.map parse

let exec _ =
    let op,vals = instructions.[ip]
    reg.[ipReg] <- ip
    execInstruction op vals
    ip <- reg.[ipReg] + 1

while instructions.Length > ip do
    exec ()

printfn "[%A] Part 1: %d %A" sw.Elapsed ip reg