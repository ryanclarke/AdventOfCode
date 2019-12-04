#load "utils/Base.fsx"
open Base

let increasingOrder password =
    password
    |> Seq.pairwise
    |> Seq.forall (fun (a,b) -> a <= b)

let containsPair pairRequirement password =
    password
    |> Seq.groupBy id
    |> Seq.map (snd >> Seq.length)
    |> Seq.exists (pairRequirement 2)

let solve min max pairRequirement =
    seq {  min .. max }
    |> Seq.map string
    |> Seq.filter increasingOrder
    |> Seq.filter (containsPair pairRequirement)
    |> Seq.length

solve 147981 691423 (<=) |> solution "04a" 1790
solve 147981 691423 (=) |> solution "04b" 1206
