let strim (s:string) = s.Trim()
let ssplit chars (s:string) = s.Split(Array.ofSeq chars)

let inputFile dataFile t =
    sprintf "input/%s.txt" dataFile
    |> System.IO.File.ReadLines
    |> Seq.map (strim >> t)

let (<*>) xs ys = 
    xs |> Seq.collect (fun x ->
        ys |> Seq.map (fun y -> (x, y)))
let square x = x <*> x

let cstring (cs:seq<char>) = cs |> Seq.toArray |> System.String

let dump (o:'a) = printfn "%A" o
let dumps s (o:'a) = printfn "%s %A" s o
let dumpf f o = f o |> dump
let dumpr o = dump o; o
let dumprs s o = dumps s o; o
let dumpcs (cs:seq<char>) = cs |> cstring |> printfn "%s"

let stopwatch = System.Diagnostics.Stopwatch.StartNew()
let dumpts _ = printfn "[%06ims]" stopwatch.ElapsedMilliseconds
let dumptsr (o:'a) = dumpts (); o
let solution puzzle expected actual =
    let result =
        if expected = actual
        then "PASS:"
        else sprintf "FAIL: expected %A but got" expected

    printfn "[%06ims] %-6s %s %A" stopwatch.ElapsedMilliseconds puzzle result actual