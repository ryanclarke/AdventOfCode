#load "utils/Base.fsx"
open Base

let meetsCriteria pairRequirement password =
    let increasing =
        sprintf "%d" password
        |> Seq.pairwise
        |> Seq.forall (fun (a,b) -> a <= b)
    
    let hasPairs =
        sprintf "%d" password
        |> Seq.groupBy id
        |> Seq.map (fun (_,v) -> Seq.length v)
        |> Seq.exists pairRequirement

    increasing && hasPairs

let solve min max pairRequirement =
    seq {
        for i in min .. max do
            if meetsCriteria pairRequirement i
            then yield i
    } |> Seq.length

solve 147981 691423 (fun x -> x >= 2) |> solution "04a" 1790
solve 147981 691423 (fun x -> x = 2) |> solution "04b" 1206
