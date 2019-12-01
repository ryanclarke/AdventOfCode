let isTestRun = false

let sw = System.Diagnostics.Stopwatch.StartNew()

let dump (format:Printf.TextWriterFormat<'T>) : 'T =
    printf "[%dms] " sw.ElapsedMilliseconds; printfn format

let calculatePower serial (x,y) =
    let rackId = x + 10
    (rackId * y + serial) * rackId % 1000 / 100 - 5

let square (x,y) size =
    [|y .. y+size-1|] |> Array.collect (fun y2 ->
        [|x .. x+size-1|] |> Array.map (fun x2 -> (x2,y2)))

let defaultGrid =
    square (1,1) 300

let createGrid serial =
    defaultGrid
    |> Array.map (fun c -> (c,(calculatePower serial c)))
    |> Map.ofArray

let squareSum (grid:Map<(int*int),int>) size origin =
    square origin size
    |> Array.sumBy (fun c -> grid.[c])

let mostPowerfulBySize grid size =
    square (1,1) (301-size)
    |> Array.maxBy (squareSum grid size)

let mostPowerful3by3 serial =
    let grid = createGrid serial
    mostPowerfulBySize grid 3

let mostPowerfulAnySize serial =
    let grid = createGrid serial
    [|1 .. 300|]
    |> Array.map (fun s -> 
        let x,y = mostPowerfulBySize grid s
        let sum = squareSum grid s (x,y)
        dump "%A = %d" (x,y,s) sum
        (x,y,s,sum))
    |> Array.maxBy (fun (_,_,_,sum) -> sum)

let solve _ =
    mostPowerful3by3 3463 |> dump "Part 1 solution: %A"
    mostPowerfulAnySize 3463 |> dump "Part 2 solution: %A"

let runTests _ =
    let check (expected:obj) (actual:obj) =
        dump "<%A> expected %A, actual %A" (expected = actual) expected actual
    
    printfn "Individual tests"
    calculatePower  8 (  3,  5) |> check  4
    calculatePower 57 (122, 79) |> check -5
    calculatePower 39 (217,196) |> check  0
    calculatePower 71 (101,153) |> check  4

    printfn "\nGrid tests"
    mostPowerful3by3 18 |> check (33,45)
    mostPowerful3by3 42 |> check (21,61)

    printfn "\nGrid size tests"
    mostPowerfulAnySize 18 |> check (90,269,16)
    mostPowerfulAnySize 42 |> check (232,251,12)

if isTestRun
then runTests ()
else solve ()