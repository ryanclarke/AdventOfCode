let sw = System.Diagnostics.Stopwatch.StartNew()

let mutable reg = [|11474091; 0; 0; 0; 0; 0|]
let mutable ip = 0
let mutable itr = 0
let mutable possibilities = Set.empty
let mutable lastPoss = -1

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

let parse input =
    let pattern = "(?<opcode>.+) (?<ints>\d+ \d+ \d+)"
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    let op = m.Groups.["opcode"].Value
    let i = m.Groups.["ints"].Value.Split(' ') |> Array.map int |> fun x -> (x.[0],x.[1],x.[2])
    (op,i)

let input = 
    System.IO.File.ReadLines("input/21.txt")
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

let dump _ =
    printfn "[%A] %d ip:%d inst:%A reg:%A" sw.Elapsed itr ip instructions.[ip] reg

let record _ =
    if possibilities.Contains reg.[3]
    then
        printfn "[%A] Part 2: %d" sw.Elapsed lastPoss
        exit 0
    else 
        lastPoss <- reg.[3]
        possibilities <- possibilities.Add lastPoss


let execInstruction opcode (a,b,c) =
    reg.[c] <- opcodes.[opcode] (a,b)

let exec f =
    while instructions.Length > ip do
        itr <- itr+1
        let op,vals = instructions.[ip]
        reg.[ipReg] <- ip
        execInstruction op vals
        f()
        ip <- reg.[ipReg] + 1

let reset _ =
    reg <- [|0; 0; 0; 0; 0; 0|]
    ip <- 0
    itr <- 0
    possibilities <- Set.empty
    lastPoss <- -1

let part1 _ =
    reset()
    reg <- [|11474091; 0; 0; 0; 0; 0|]
    exec id
    printfn "[%A] Part 1: %d ip:%d reg:%A" sw.Elapsed itr ip reg
 
let part2 _ =
    reset()
    exec (fun _ -> if ip=16 then record() else ())

part1()
part2()
