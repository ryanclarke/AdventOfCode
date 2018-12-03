let freqs = System.IO.File.ReadLines("input/1.txt") |> Seq.map int |> Seq.toArray
let freqLength = freqs |> Array.length
let getFreq index =
    freqs.[index % freqLength]
    
let lookForDuplicate =
    Seq.unfold (fun state ->
        let found, index, list = state
        if (found) then
            None
        else 
            let freq = freqs.[index % (freqs |> Array.length)]
            let newList = 
                match list with
                | [] -> [ freq ]
                | x :: _ -> freq + x :: list
            
            if (list |> List.contains (newList |> List.head)) then
                Some ((true, index+1, newList), (true, index+1, newList))
            else
                Some (state, (found, index+1, newList))) (false, 0, [0])

lookForDuplicate |> Seq.last |> printfn "%A"