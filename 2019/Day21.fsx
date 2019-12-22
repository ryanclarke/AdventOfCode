#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let program =
    inputFile "21" string
    |> Seq.exactlyOne
    |> ssplit ","
    |> Array.map int64

let solve text =
    let input = 
        text
        |> Seq.map (char >> int)
        |> Seq.toList
        |> List.filter (fun i -> i <> 13)
        |> List.tail
    let output = (IntcodeComputer.run input program)
    output |> List.rev |> List.map char |> cstring |> dump
    output.Head |> int

// ===================================================================================================================
// 2019.21 A
// ===================================================================================================================

"
NOT A T
AND A T
NOT A J
OR J T
NOT B J
OR J T
NOT C J
OR J T
AND D T
NOT D J
AND D J
OR T J
WALK
" |> solve |> solution "21a" 19362259

// ===================================================================================================================
// 2019.21 B
// ===================================================================================================================

let testJump expected s = 
    let (J,T,A,B,C,D,E,F,G,H,I) = (0,1,2,3,4,5,6,7,8,9,10)
    let r = false::false::(s |> Seq.map (fun c -> c = '#') |> Seq.toList) |> Array.ofList |> dumpr
    
    let AND x y = r.[y] <- r.[x] && r.[y]
    let NOT x y = r.[y] <- not r.[x]
    let OR x y = r.[y] <- r.[x] || r.[y]

    let setFalse y =
        NOT A y
        AND A y

    let setTrue y =
        NOT A y
        OR A y
 
    setFalse T

    NOT A T
    AND A T
    NOT D J
    OR J T
    NOT H J
    OR J T
    NOT C J
    OR J T
    AND D T
    NOT A J
    AND A J
    OR E J
    OR H J
    AND T J


    printfn "%b %b" (r.[J] = expected)  r.[J]



testJump false "####.#.##" // ####.#.##.#.####
testJump false "####..#.#" // #####..#.########




"
NOT A J
OR J T
NOT D J
OR J T
NOT H J
OR J T
NOT C J
OR J T
AND D T
NOT A J
AND A J
OR E J
OR H J
AND T J
RUN
" |> solve |> solution "21b" 0