#load "utils/Base.fsx"
open Base

let parse file =
    inputFile file string
    |> Seq.toArray
    |> Array.collect (Array.ofSeq)
    |> Array.map (fun c -> if c = '#' then 1 else 0)

// ===================================================================================================================
// 2019.24 A
// ===================================================================================================================

let solveA file =
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

    let rec simulateMinute (history:int list) (minute:int array) =
        let biodiversity = minute |> Array.mapi (fun i t -> (pown 2 i) * t) |> Array.sum
        let newMinute = Array.map2 (calcTile minute) minute adjacents
        match history |> List.contains biodiversity with
        | true -> biodiversity
        | false -> simulateMinute (biodiversity::history) newMinute

    let startingMinute = parse file
    simulateMinute [] startingMinute

solveA "24test"
|> solution "24a.1" 2129920
solveA "24"
|> solution "24a" 32509983

// ===================================================================================================================
// 2019.24 B
// ===================================================================================================================

//    00 01 02 03 04    25 26 27 28 29    50 51 52 53 54
//    05 06 07 08 09    30 31 32 33 34    55 56 57 58 59
//    10 11    13 14    35 36    38 39    60 61    63 64
//    15 16 17 18 19    40 41 42 43 44    65 66 67 68 69
//    20 21 22 23 24    45 46 47 48 49    70 71 72 73 74

let solveB time file =
    let size = 25
    let totalSize = size * time * 2
    let adjecent i =
        match i % size with
        | 0 -> [-18;-14;1;5]
        | 1 -> [-19;-1;1;5]
        | 2 -> [-20;-1;1;5]
        | 3 -> [-21;-1;1;5]
        | 4 -> [-22;-1;-16;5]
        | 5 -> [-5;-19;1;5]
        | 6 -> [-5;-1;1;5]
        | 7 -> [-5;-1;1;18;19;20;21;22]
        | 8 -> [-5;-1;1;5]
        | 9 -> [-5;-1;-21;5]
        | 10 -> [-5;-24;1;5]
        | 11 -> [-5;-1;14;19;24;29;34;5]
        | 12 -> []
        | 13 -> [-5;16;21;26;31;36;1;5]
        | 14 -> [-5;-1;-26;5]
        | 15 -> [-5;-29;1;5]
        | 16 -> [-5;-1;1;5]
        | 17 -> [28;29;30;31;32;-1;1;5]
        | 18 -> [-5;-1;1;5]
        | 19 -> [-5;-1;-31;5]
        | 20 -> [-5;-34;1;-28]
        | 21 -> [-5;-1;1;-29]
        | 22 -> [-5;-1;1;-30]
        | 23 -> [-5;-1;1;-31]
        | 24 -> [-5;-1;-36;-32]
        | err -> failwithf "Invalid position: %A" err
        |> List.map (fun offset -> i + offset)
        |> List.filter (fun x -> x >= 0 && x < totalSize)

    let adjacents = [| for i in 0 .. (totalSize-1) do adjecent i |]

    let calcTile (bugMap:int[]) (tile:int) (adj:int list) =
        match tile,adj |> List.sumBy (fun a -> bugMap.[a]) with
        | 1,1 | 0,1 | 0,2 -> 1
        | _,_ -> 0

    let rec simulateMinute minute (map:int array) =
        match minute = time with
        | true -> Array.sum map
        | false -> 
            let newMap = Array.map2 (calcTile map) map adjacents
            simulateMinute (minute+1) newMap

    let startingMap =
        Array.concat [
            Array.zeroCreate (totalSize/2-size);
            parse file;
            Array.zeroCreate (totalSize/2)
        ]
    simulateMinute 0 startingMap

solveB 10 "24test"
|> solution "24b.1" 99
solveB 200 "24"
|> solution "24b" 2012