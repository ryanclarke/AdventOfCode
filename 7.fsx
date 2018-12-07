#load "Utils.fsx"
open Utils

let input =
    aocInput 7
    |> Seq.map (fun x -> x.[5],x.[36])
    |> Seq.sortBy fst
    |> Seq.toList

let eligable (steps:(char*char) list) =
    steps
    |> Seq.map fst
    |> Seq.except (steps |> Seq.map snd)
    |> Seq.toList

let rec folder (state:char list) steps =
    match eligable steps with
    | x :: tail ->
        match steps with
        | [] -> state
        | [z] -> snd steps.Head :: (folder (x :: state) (steps |> List.filter (fun y -> (fst y) <> x)))
        | _ -> folder (x :: state) (steps |> List.filter (fun y -> (fst y) <> x))
    | [] -> state

folder [] input |> Seq.rev |> dumpcs

let reference f =
    input
    |> Seq.map f
    |> Seq.sort
    |> Seq.distinct
    |> dumpcs
reference fst
reference snd