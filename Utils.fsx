let aocInput i =
    sprintf "input/%d.txt" i
    |> System.IO.File.ReadLines
    |> Seq.map (fun x -> x.Trim())

let (*) a b = 
    a |> Seq.collect (fun x ->
        b |> Seq.map (fun y -> (x, y)))
let square a = a * a

let dump (o:'a) = printfn "%A" o
let dumpf f o = f o |> dump
let dumpr o = dump o; o
let dumpcs (cs:seq<char>) = cs |> Seq.toArray |> System.String |> dump