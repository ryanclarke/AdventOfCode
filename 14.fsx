let sw = System.Diagnostics.Stopwatch.StartNew()

let run after =
    let times = after + 10
    let mutable scores = Array.zeroCreate (times+1)
    scores.[0] <- 3
    scores.[1] <- 7
    scores.[2] <- 1
    scores.[3] <- 0
    let mutable a = 0
    let mutable b = 1
    let mutable length = 4

    let move i =
        let offset = (i + (scores.[i] + 1))
        if offset >= length
        then offset - length
        else offset
        
    while length < times do
        (scores.[a] + scores.[b])
        |> fun i -> if i >= 10 then [i/10; i%10] else [i]
        |> List.mapi (fun i s -> scores.[length+i] <- s)
        |> fun s -> length <- length + s.Length
        a <- move a
        b <- move b
    scores

let output expected actual =
    printfn "[%A] -- %A -- exp: %A act: %A" sw.Elapsed (expected=actual) expected actual

let part1 after expected =
    (run after).[after..]
    |> Array.take 10
    |> Array.map (fun i -> i.ToString())
    |> fun s -> System.String.Join("", s)
    |> output expected

printfn "Part 1"
part1 5 "0124515891"
part1 9 "5158916779"
part1 18 "9251071085"
part1 2018 "5941429882"
part1 503761 "1044257397" // Solution to part 1

let result =
    (run 210000000) |> Array.toSeq

let part2 (find:int[]) expected =
    result
    |> Seq.windowed find.Length
    |> Seq.findIndex (fun x -> x = find)
    |> output expected

printfn "Part 2"
part2 [|0;1;2;4;5|] 5
part2 [|5;1;5;8;9|] 9
part2 [|9;2;5;1;0|] 18
part2 [|5;9;4;1;4|] 2018
part2 [|5;0;3;7;6;1|] 20185425 // Solution to part 2