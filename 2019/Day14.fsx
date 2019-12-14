#load "utils/Base.fsx"
open Base

type Batch = {
    Chem: string
    Need: int64
}

let parseBatch text =
    text |> strim |> ssplit " " |> fun batch ->
        match batch with
        | [|one;two|] -> {Chem = strim two; Need = strim one |> int64}
        | err -> failwithf "Invalid batch input: %A" err


let parse line =
    line |> ssplit "=>" |> fun line ->
        match line with
        | [|one;two|] ->
            let result = two |> parseBatch
            let ingredients = one |> ssplit "," |> Array.map parseBatch |> Array.toList
            result.Chem,(result,ingredients)
        | err -> failwithf "Invalid line input: %A" err

// ===================================================================================================================
// 2019.14 A
// ===================================================================================================================

let solveA input fuelCount =
    let reactions =
        inputFile input string
        |> Seq.map parse
        |> Seq.toList
        |> List.append ["ORE",({Chem = "ORE"; Need = 1L},[])] 
    let reactionsMap = reactions |> Map.ofList
    let chems =
        reactions
        |> List.map (snd >> fun (chem,ingrs) ->
            (chem.Chem, ingrs |> List.map (fun chem -> chem.Chem)))
    let chemMap = chems |> Map.ofList |> Map.add "ORE" []
    let chemSort chemA chemB =
        match chemA,chemB with
        | (_,ingr),(chem,_) when ingr |> List.contains chem -> 1
        | (chem,_),(_,ingr) when ingr |> List.contains chem -> -1
        | _ -> if (snd chemA |> List.length) > (snd chemB |> List.length) then 1 else -1

    let rec collectIngredients ingrs =
        match ingrs with
        | [] | ["ORE"] -> ingrs
        | chems -> chems |> List.collect (fun chem ->
            let ingrs = chemMap.[chem] |> collectIngredients
            chem::ingrs
        ) |> List.distinct

    let foldState = ["FUEL",(int64 fuelCount)] |> Map.ofList
    let folder (state:Map<string,int64>) (chem:string) =
        let addToNeeds runs state batch =
            let newNeed =
                if (state |> Map.containsKey batch.Chem)
                then batch.Need * runs + state.[batch.Chem]
                else batch.Need * runs
            state |> Map.add batch.Chem newNeed

        let need = state.[chem]
        let batch,ingredients = reactionsMap.[chem]
        let runs = (need / batch.Need) + (if need % batch.Need = 0L then 0L else 1L)
        ingredients |> List.fold (addToNeeds runs) state

    chems
    |> List.map (fun (chem,ingrs) -> (chem,collectIngredients ingrs))
    |> List.sortWith chemSort
    |> List.rev
    |> List.map fst
    |> List.fold folder foldState
    |> fun x -> x.["ORE"]


solveA "14test1" 1L
|> solution "14a.1" 13312L
solveA "14test2" 1L
|> solution "14a.2" 180697L
solveA "14test3" 1L
|> solution "14a.3" 2210736L
solveA "14" 1L
|> solution "14a" 136771L

// ===================================================================================================================
// 2019.14 B
// ===================================================================================================================


let solveB input =
    let mutable change = 1_000_000_000_000L
    let mutable fuel = 0L
    while change > 0L do
        let guessFuel = fuel + change
        let ore = solveA input guessFuel
        if ore > 1_000_000_000_000L
        then change <- change / 10L
        else fuel <- guessFuel
    fuel

solveB "14test1"
|> solution "14b.1" 82892753L
solveB "14test2"
|> solution "14b.2" 5586022L
solveB "14test3"
|> solution "14b.3" 460664L
solveB "14"
|> solution "14b" 8193614L