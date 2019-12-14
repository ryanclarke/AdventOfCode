#load "utils/Base.fsx"
open Base

let example1 ="157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
                |> ssplit "\n"

let example2 ="2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF"
                |> ssplit "\n"

let example3 ="171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX"
                |> ssplit "\n"

let puzzle =
    inputFile "14" string

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

let solveA description fuelCount =
    let reactions =
        description
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


solveA example1 1L
|> solution "14a.1" 13312L
solveA example2 1L
|> solution "14a.2" 180697L
solveA example3 1L
|> solution "14a.3" 2210736L
solveA puzzle 1L
|> solution "14a" 136771L

// ===================================================================================================================
// 2019.12 B
// ===================================================================================================================


let solveB description =
    let mutable change = 1_000_000_000_000L
    let mutable fuel = 0L
    while change > 0L do
        let guessFuel = fuel + change
        let ore = solveA description guessFuel
        if ore > 1_000_000_000_000L
        then change <- change / 10L
        else fuel <- guessFuel
    fuel

solveB example1
|> solution "14b.1" 82892753L
solveB example2
|> solution "14b.2" 5586022L
solveB example3
|> solution "14b.3" 460664L
solveB puzzle
|> solution "14b" 8193614L