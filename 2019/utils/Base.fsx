let sreplace (oldString:string) (newString:string) (s:string) = s.Replace (oldString,newString)
let ssplit (splitString:string) (s:string) = s.Split ([|splitString|], System.StringSplitOptions.RemoveEmptyEntries)
let strim (s:string) = s.Trim()

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
let dumpts _ = printfn "[% 6i ms]" stopwatch.ElapsedMilliseconds
let dumptsr (o:'a) = dumpts (); o
let dumptso (o:'a) = printfn "[% 6i ms] %A" stopwatch.ElapsedMilliseconds o
let dumptss (s:string) = printfn "[% 6i ms] %s" stopwatch.ElapsedMilliseconds s
let solution puzzle expected actual =
    let result =
        if expected = actual
        then "PASS:"
        else sprintf "FAIL: expected %A but got" expected

    printfn "[% 6i ms] %-6s %s %A" stopwatch.ElapsedMilliseconds puzzle result actual