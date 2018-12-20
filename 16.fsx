let opcodes =
    let addr (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] + reg.[b] else x)
    let addi (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] + b else x)
    let mulr (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] * reg.[b] else x)
    let muli (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] * b else x)
    let banr (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] &&& reg.[b] else x)
    let bani (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] &&& b else x)
    let borr (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] ||| reg.[b] else x)
    let bori (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] ||| b else x)
    let setr (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then reg.[a] else x)
    let seti (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then a else x)
    let gtir (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then (if a > reg.[b] then 1 else 0) else x)
    let gtri (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then (if reg.[a] > b then 1 else 0) else x)
    let gtrr (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then (if reg.[a] > reg.[b] then 1 else 0) else x)
    let eqir (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then (if a = reg.[b] then 1 else 0) else x)
    let eqri (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then (if reg.[a] = b then 1 else 0) else x)
    let eqrr (a,b,c) reg = reg |> Array.mapi (fun i x -> if i = c then (if reg.[a] = reg.[b] then 1 else 0) else x)

    [("addr",addr);
     ("addi",addi);
     ("mulr",mulr);
     ("muli",muli);
     ("banr",banr);
     ("bani",bani);
     ("borr",borr);
     ("bori",bori);
     ("setr",setr);
     ("seti",seti);
     ("gtir",gtir);
     ("gtri",gtri);
     ("gtrr",gtrr);
     ("eqir",eqir);
     ("eqri",eqri);
     ("eqrr",eqrr)]
    |> Map.ofList

let numberOfMatches before instruction after =
    opcodes
    |> Seq.filter (fun kvp -> (kvp.Value) (snd instruction) before = after)
    |> Seq.map (fun kvp -> kvp.Key)
    |> fun s ->
        let op,_ = instruction
        (op,s)

let parseSamples input =
    let pattern = "Before: \[(?<before>.+)\] (?<instruction>.+) After:  \[(?<after>.+)\]"
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    let b = m.Groups.["before"].Value.Replace(",", "").Split([|' '|]) |> Array.map int
    let i = m.Groups.["instruction"].Value.Split(' ') |> Array.map int |> fun x -> (x.[0],(x.[1],x.[2],x.[3]))
    let a = m.Groups.["after"].Value.Replace(",", "").Split([|' '|]) |> Array.map int
    numberOfMatches b i a

let samples =
    System.IO.File.ReadLines("input/16samples.txt")
    |> Seq.chunkBySize 4
    |> Seq.map (fun x -> System.String.Join(" ", x) |> parseSamples)

samples
|> Seq.filter (fun (k,v) -> v |> Seq.length >= 3)
|> Seq.length
|> printfn "Part 1: %d"

let opLookup =
    let rec trimOptions (x:seq<int*(string list)>) =
        x
        |> Seq.sortBy (fun (k,v) -> v.Length)
        |> Seq.fold (fun state ops -> 
            let (disc:Set<string>),o = state
            let op,s = ops
            let newS = s |> Seq.filter (fun x -> disc.Contains x |> not) |> Seq.toList
            if (newS.Length = 1)
            then ((disc.Add s.Head),(op,newS)::o)
            else (disc,(op,newS)::o)
            ) (Set.empty,[])
        |> fun (a,b) -> b
        |> fun a ->
            let remaining = a |> List.filter (fun (_,b) -> b.Length > 1)
            if remaining |> List.isEmpty
            then a
            else trimOptions a

    samples
    |> Seq.groupBy (fun (k,_) -> k)
    |> Seq.map (fun (k,v) ->
        (k, (v 
            |> Seq.map (fun (_,s) -> s |> Set.ofSeq)
            |> Set.intersectMany)))
    |> Seq.map (fun (k,v) -> (k,v |> Seq.toList))
    |> trimOptions
    |> Seq.map (fun (k,v) -> (k,opcodes.[v |> List.exactlyOne]))
    |> Map.ofSeq

let folder reg instruction =
    let op,vals = instruction
    opLookup.[op] vals reg

System.IO.File.ReadLines("input/16program.txt")
|> Seq.map (fun s ->
    s.Split(' ') |> Array.map int |> fun x -> (x.[0],(x.[1],x.[2],x.[3])))
|> Seq.fold folder [|0;0;0;0|]
|> printfn "Part 2 : %A"
