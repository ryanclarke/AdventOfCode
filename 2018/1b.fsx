let freqs = System.IO.File.ReadLines("input/1.txt") |> Seq.map int |> Seq.toArray
let nextFreq i = freqs.[i % freqs.Length]

let lookForDuplicate =
    let finder state =
        let found, sum, index, (set:Set<int>) = state
        if found then
            None
        else
            let newSum = sum + nextFreq index
            Some (newSum, (set.Contains newSum, newSum, index+1, set.Add newSum))
    Seq.unfold finder (false, 0, 0, Set.empty)

lookForDuplicate |> Seq.last |> printfn "%A"