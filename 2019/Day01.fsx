#load "utils/Base.fsx"
open Base

let moduleMasses = inputFile "01" int

let massToFuel mass =
    mass / 3 - 2

moduleMasses
|> Seq.sumBy massToFuel
|> solution "01a"


let rec fuelForFuelMass (fuels:List<int>) =
    match fuels with
    | x::_ when x >= 9 -> fuelForFuelMass ((massToFuel x)::fuels)
    | x -> x

let massToFuelWithFuelMass mass =
    [ massToFuel mass ]
    |> fuelForFuelMass
    |> List.sum

moduleMasses
|> Seq.sumBy massToFuelWithFuelMass
|> solution "01b"