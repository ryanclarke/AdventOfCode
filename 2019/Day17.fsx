#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let program file =
    inputFile file string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

let crossArea (x,y) = [
    (x,y)
    (x,y-1)
    (x,y+1)
    (x+1,y)
    (x-1,y)
]

let cimage file =
        IntcodeComputer.run [] (program file)
        |> List.rev
        |> List.map char
        |> cstring
        |> ssplit "\n"

let test = "..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^.." |> ssplit "\n"


let dumpImage arrayxy =
    let c i = (i % 10) |> sprintf "%i" |> char
    printfn "   00000000001111111111222222222233333333334"
    arrayxy |> Array.mapi (fun i ys -> ys |> Array.mapi (fun i x -> if x = '#' then (c i) else x) |> cstring |> printfn "%2i %s" i) |> ignore
    printfn "   00000000001111111111222222222233333333334"

// ===================================================================================================================
// 2019.17 A
// ===================================================================================================================

let solveA input =
    let image =
        input
        |> Array.map (Seq.toArray)
    
    let height = image.Length
    let width = image.[0].Length

    let isScaffold (x,y) =
        not (x < 0 || x >= width || y < 0 || y >= height || image.[y].[x] <> '#')
    
    //dumpImage image |> ignore
    image |> Array.mapi (fun y yChars ->
        yChars |> Array.mapi (fun x xChar -> 
            if crossArea (x,y) |> List.forall isScaffold
            then Some (x,y)
            else None
        ))
    |> Array.collect (Array.choose id)
    |> Array.sumBy (fun (x,y) -> x*y) //printfn "%2i * %2i = %3i" x y (x*y); x * y)

solveA test
|> solution "17a.1" 76
solveA (cimage "17test1")
|> solution "17a.2" 7816
solveA (cimage "17")
|> solution "17a" 2508

// ===================================================================================================================
// 2019.17 B
// ===================================================================================================================

let solveB file =
    let input = "A,B,A,B,A,C,B,C,A,C
L,10,L,12,R,6
R,10,L,4,L,4,L,12
L,10,R,10,R,6,L,4
N
"               |> Seq.map (char >> int64)
                |> Seq.toList
                |> List.filter (fun i -> i <> 13L)
                
    let mutable memory = program file
    memory.[0] <- 2L
    let output = IntcodeComputer.run input memory
    output.Head |> int

solveB "17"
|> solution "17b" 799463