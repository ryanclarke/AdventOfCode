#load "utils/Base.fsx"
open Base
#load "utils/ICC.fsx"
open ICC

let parse file =
    inputFile file string
    |> Seq.toArray
    |> Array.collect (Array.ofSeq)
    |> Array.map (fun c -> if c = '#' then 1 else 0)

let adjecent i =
    [ for offset in [-5; -1; 1; 5] do 
        let a = i + offset
        let inBounds = a >= 0 && a <= 24
        let ns = (abs (a/5 - i/5) = 1) && a%5 = i%5
        let ew = a/5 = i/5 && (abs (a%5 - i%5) = 1)
        if inBounds && (ns || ew) then yield a
    ]

let adjacents = [| for i in 0 .. 24 do adjecent i |]

let calcTile (bugMap:int[]) (tile:int) (adj:int list) =
    match tile,adj |> List.sumBy (fun a -> bugMap.[a]) with
    | 1,1 | 0,1 | 0,2 -> 1
    | _,_ -> 0

// ===================================================================================================================
// 2019.24 A
// ===================================================================================================================

let rec simulateMinute (history:int list) (minute:int array) =
    let biodiversity = minute |> Array.mapi (fun i t -> (pown 2 i) * t) |> Array.sum
    let newMinute = Array.map2 (calcTile minute) minute adjacents
    match history |> List.contains biodiversity with
    | true -> biodiversity
    | false -> simulateMinute (biodiversity::history) newMinute

let solveA file =
    let startingMinute = parse file
    simulateMinute [] startingMinute

solveA "24test"
|> solution "24a.1" 2129920
solveA "24"
|> solution "24a" 32509983

// ===================================================================================================================
// 2019.24 B
// ===================================================================================================================

let solveB _ = 0

solveB "24"
|> solution "24b" 0