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
        |> Seq.map (char >> int64)
        |> Seq.toList
        |> List.filter (fun i -> i <> 13L)
        |> List.tail
    let output = (IntcodeComputer.run input program)
    //output |> List.rev |> List.collect (fun i -> if i > 256L then string i |> List.ofSeq else [char i]) |> cstring |> dump
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

"
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT E T
AND E T
OR E T
OR H T
AND T J
RUN
" |> solve |> solution "21b" 1141066762

let testJump shouldJump s = 
    let (J,T,A,B,C,D,E,F,G,H,I) = (0,1,2,3,4,5,6,7,8,9,10)
    let r = false::false::(s |> Seq.map (fun c -> c = '#') |> Seq.toList) |> Array.ofList
    
    let AND x y = r.[y] <- r.[x] && r.[y]
    let NOT x y = r.[y] <- not r.[x]
    let OR x y = r.[y] <- r.[x] || r.[y]

    NOT A J
    NOT B T
    OR T J
    NOT C T
    OR T J
    AND D J
    NOT E T
    AND E T
    OR E T
    OR H T
    AND T J

    r.[J] = shouldJump,s

[
(false,"####.#.##");
(false,"####..#.#");
(false,"..#.#####");
(false,"###..#.##");
(false,"##..#.###");
(false,"##.#.##.#");
(true, ".#.##.#.#");
(true, "#.##...##");
]
|> List.map (fun (exp,s) -> testJump exp s)
// |> dumpr
|> List.forall fst
|> solution "21b.1" true
