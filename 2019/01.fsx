#load "Utils.fsx"
open Utils

let moduleMasses =
    aocInput "01"
    |> Seq.map int

let massToFuel mass =
    mass / 3 - 2

moduleMasses
|> Seq.sumBy massToFuel
|> dumps "[01a] Sum of fuel requirements:"


let rec fuelWithFuel (fuels:List<int>) =
    match fuels with
    | x::_ when x >= 9 -> fuelWithFuel ((massToFuel x)::fuels)
    | x -> x

let massWithFuel mass =
    [ massToFuel mass ]
    |> fuelWithFuel
    |> List.sum

moduleMasses
|> Seq.sumBy massWithFuel
|> dumps "[01b] Sum of fuel requirements, counting fuel weight:"