let input =
    System.IO.File.ReadLines "input/7.txt"
    |> Seq.map (fun x -> x.[5],x.[36])
    |> Seq.sortBy fst
    |> Seq.toList

let eligible (steps:(char*char) list) =
    steps
    |> Seq.filter (fun (x,_) -> steps |> Seq.map snd |> Seq.contains x |> not)
    |> Seq.toList

let rec folder state steps =
    let recurse x = folder (x :: state) (steps |> List.filter (fun (z,_) -> z <> x))
    match eligible steps with
    | (x,y) :: _ ->
        match steps with
        | [] -> state
        | [_] -> y :: recurse x
        | _ -> recurse x
    | [] -> state

folder [] input |> Seq.rev |> Seq.toArray |> System.String |> printfn "%A"
printfn "======"

type Step = {
    Task: char
    Prereqs: char list
    AvailableAt: int
} with member this.Duration = 
        int this.Task - 4

let duration (step:Step) =
    step.Duration

let stepList =
    input
    |> List.groupBy snd
    |> List.map (fun (k,v) -> (k,List.map fst v))
    |> fun xs ->
        seq { 'A' .. 'Z' }
        |> List.ofSeq
        |> List.except (xs |> List.map fst)
        |> List.map (fun x -> (x,List.empty))
        |> List.append xs
    |> List.sortBy fst
    |> List.map (fun (k,v) -> {Task = k; Prereqs = v; AvailableAt = 0})
    //|> List.map (fun (k,v) -> printfn "%A %A: %A" k (int k - 4) (cstring v); (k,v))

input
|> List.map snd
|> List.except (input |> List.map fst)
|> List.exactlyOne

let availableSteps sl =
    sl
    |> List.sortBy (fun x -> ((x.Prereqs |> List.length) * 100 + x.Duration))

let firstAvailableStep sl =
    sl
    |> availableSteps
    |> List.tryHead

type Elf = {
    Tasks: int list
    IsIdle: bool
}
let newElf _ = {Tasks = []; IsIdle = true}

let availableAt elf =
    elf.Tasks |> List.sum

let available elfs =
    elfs
    |> List.sortBy availableAt

let assign elfs task =
    match available elfs with
    | elf::others ->
        let newElf = {Tasks = (duration task)::elf.Tasks; IsIdle = false}
        let newOthers = 
            others
            |> List.map (fun e -> 
                let diff = availableAt elf - availableAt e
                if diff > 0
                then {e with Tasks = diff::e.Tasks}
                else e)
        newElf::newOthers
    | [] -> failwith "Why no elfs?"

let update (sl:Step list) (step:Step) elf =
    sl
    |> List.map (fun s ->
        if s.Task = step.Task
        then {step with AvailableAt = availableAt elf + step.Duration}
        else s)

[ 0 .. 25 ]
|> List.fold (fun (state:(Elf list*Step list)) _ -> 
        let elfs,sl = state
        match elfs |> available with
        | elf::others -> 
            match firstAvailableStep sl with
            | Some step -> 
                let newSl = update sl step elf
                let newElfs = assign elfs step
                (newElfs,newSl)
            | None -> state
        | [] -> failwith "Oops"
    ) (List.init 5 newElf,stepList)
|> fst
|> available
|> List.map availableAt
|> printfn "%A"




// let rec allPaths cs (c:char) =
//     match taskMap.[c] with
//     | [] -> dumpcs (c::cs); c::cs
//     | x -> List.map (allPaths c::cs) x

// 'E'
// |> allPaths []





printfn "======"

let reference f =
    input
    |> Seq.map f
    |> Seq.sort
    |> Seq.distinct
    |> Seq.toArray
    |> System.String
    |> printfn "%A"
reference fst
reference snd