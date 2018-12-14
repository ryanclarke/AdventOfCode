let intl i =
    if i >= 10 then [i/10; i%10] else [i]


let run after =
    let times = after + 10
    let sw = System.Diagnostics.Stopwatch.StartNew()
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
        
    // printfn "%d %d %d %A" a b length scores
    while length < times do
        let newScores =
            (scores.[a] + scores.[b])
            |> intl
        newScores
        |> List.iteri (fun i s -> scores.[length+i] <- s)
        length <- length + newScores.Length
        a <- move a
        b <- move b
        if length % 10000 = 0
        then printfn "[%A] - %d" sw.Elapsed length
        // printfn "%d %d %d %A" a b length scores
    
    scores.[after..]
    |> Array.take 10
    |> Array.map (fun i -> i.ToString())
    |> (fun s ->
        System.String.Join("", s)
    )




let test after expected =
    let exp = expected
    run after
    |> fun act -> printfn "[%A] exp: %A act: %A" (exp=act) exp act
    

test 5 "0124515891"
test 9 "5158916779"
test 18 "9251071085"
test 2018 "5941429882"
test 503761 "1044257397" // Solution to part 1