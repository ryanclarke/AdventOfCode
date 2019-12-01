let aocInput x =
    sprintf "input/%s.txt" x
    |> System.IO.File.ReadLines
    |> Seq.map (fun l -> l.Trim())

let (<*>) a b = 
    a |> Seq.collect (fun x ->
        b |> Seq.map (fun y -> (x, y)))
let square a = a <*> a

let dump (o:'a) = printfn "%A" o
let dumps s (o:'a) = printfn "%s %A" s o
let dumpf f o = f o |> dump
let dumpr o = dump o; o
let dumprs s o = dumps s o; o
let cstring (cs:seq<char>) = cs |> Seq.toArray |> System.String
let dumpcs (cs:seq<char>) = cstring cs |> dump


dump (System.Guid.NewGuid())
