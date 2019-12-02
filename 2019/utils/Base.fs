namespace Aoc
module Base =
    let strim (s:string) = s.Trim()
    let ssplit chars (s:string) = s.Split(chars)

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
    let dumpcs (cs:seq<char>) = cs |> cstring |> dump

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let solution puzzle answer = printfn "[%06ims] %s: %A" stopwatch.ElapsedMilliseconds puzzle answer