let rotate l =
    (l |> List.last) :: (l |> List.take (l.Length-1))

let backRotate (l:int list) =
    let eight = l.[..7]
    eight.[7], eight.[6] :: l.[8..] @ eight.[0..5]

let takeTurn i state marble =
    let scores,marbles = state
    if marble % 23 = 0
    then
        let s,m = backRotate marbles
        let player = marble % (scores |> Map.count)
        let newScores = scores |> Map.add player (scores.[player] + marble + s)
        (newScores,m)
    else (scores,marble::rotate marbles)

let solve players lastMarble =
    let scores =
        seq { 0 .. players-1}
        |> Seq.map (fun x -> (x,0))
        |> Map.ofSeq
    seq { 1 .. lastMarble }
    |> Seq.fold (takeTurn lastMarble) (scores,[0])
    |> fst
    |> Map.toList
    |> List.maxBy snd
    |> snd

let scenario players lastMarble highScore =
    solve players lastMarble
    |> printfn "%d players, %d highest marble = high score %d, got %A" players lastMarble highScore

// scenario 9 25 32
// scenario 10 1618 8317
// scenario 13 7999 146373
// scenario 17 1104 2764
// scenario 21 6111 54718
// scenario 30 5807 37305
scenario 411 72059 0