#load "utils/Base.fsx"
open Base

let meetsCriteria password =
    let digits =
        sprintf "%d" password
        |> Seq.map (string >> int)
        |> Seq.pairwise
    
    let increasing (a,b) = a <= b
    let isPair (a,b) = a = b

    digits |> Seq.forall increasing
    && digits |> Seq.exists isPair

let solve min max =
    seq {
        for i in min .. max do
            if meetsCriteria i
            then yield i
    } |> Seq.length

solve 123440 123460 |> solution "04a test" 7
solve 147981 691423 |> solution "04a" 1790
