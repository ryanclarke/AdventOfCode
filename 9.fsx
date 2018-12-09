open System.Diagnostics

type Marbles = {
    Front: int list
    Back: int list
}

let startMarbles = {Front = []; Back = [0]}

let normalPlay marbles marble = 
    let refillBack marbles =
        {Front = []; Back = List.rev marbles.Front}

    let rec rotate marbles =
        match marbles.Back with
        | x::rest -> {Front = x::marbles.Front; Back = rest}
        | [] -> refillBack marbles |> rotate
       
    let place marble marbles =
        {marbles with Front = marble::marbles.Front}

    rotate marbles |> place marble

let scoringPlay marbles =
    let refillFront marbles =
        let desired = 8 - marbles.Front.Length
        let splitPoint = marbles.Back.Length - desired
        marbles.Back
        |> List.splitAt splitPoint
        |> fun (newBack,toFront) ->
            {Front = toFront |> List.fold (fun m x -> x::m) marbles.Front; Back = newBack}

    let rec rotateBack marbles =
        match marbles.Front with
        | a::b::c::d::e::f::g::h::rest ->
            (h,{Front = g::rest; Back = f::e::d::c::b::a::marbles.Back})
        | _ ->
            refillFront marbles |> rotateBack

    rotateBack marbles

let takeTurn state marble =
    let scores,marbles = state
    if marble % 23 = 0
    then
        let s,m = scoringPlay marbles
        let player = marble % (scores |> Map.count)
        let newScores = scores |> Map.add player (scores.[player] + (int64 marble) + (int64 s))
        (newScores,m)
    else (scores,normalPlay marbles marble)

let solve players lastMarble =
    let scores =
        seq { 0 .. players-1}
        |> Seq.map (fun x -> (x, int64 0))
        |> Map.ofSeq
    seq { 1 .. lastMarble }
    |> Seq.fold takeTurn (scores,startMarbles)
    |> fst
    |> Map.toList
    |> List.maxBy snd
    |> snd

let scenario players lastMarble highScore =
    let start = Stopwatch.StartNew()
    solve players lastMarble
    |> printfn "[%d ms] %d players, %d highest marble = %d" start.ElapsedMilliseconds players lastMarble

scenario 9 25 32
scenario 10 1618 8317
scenario 13 7999 146373
scenario 17 1104 2764
scenario 21 6111 54718
scenario 30 5807 37305
scenario 411 72059 0
scenario 411 7205900 0