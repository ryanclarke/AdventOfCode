let sw = System.Diagnostics.Stopwatch.StartNew()
let dump s o = printf "[%A] " sw.Elapsed; printfn s o

type Army = ImmuneSystem | Infection | Neutral
type DamageType = DamageType of string

type Cohort = {
    Army: Army
    Units: int
    HitPoints: int
    Immunities: Set<DamageType>
    Weaknesses: Set<DamageType>
    Damage: int
    DamageType: DamageType
    Initiative: int
}
with
    member this.EffectivePower = this.Units * this.Damage
    member this.DamageMultiplier d =
        match d with
        | _ when this.Immunities.Contains d -> 0
        | _ when this.Weaknesses.Contains d -> 2
        | _ -> 1
    member this.attackDamageAgainst (d:Cohort) =
        this.EffectivePower * (d.DamageMultiplier this.DamageType)
    member this.attackedBy (a:Cohort) =
        let killed = ((a.attackDamageAgainst this) / this.HitPoints)
        {this with Units = max 0 (this.Units - killed)}

let mutable army = Neutral
let parse line =
    let parse pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if not m.Success
        then None
        else
            [ 0 .. m.Groups.Count-1 ]
            |> List.map (fun i -> m.Groups.[i].Value)
            |> Some

    let extractDamageTypes (str:string list option) =
        match str with
        | None -> None
        | Some s ->
            s.[1].Replace(")","").Split(',')
            |> Array.map (fun d -> DamageType (d.Trim()))
            |> Set.ofArray
            |> Some

    let (|Immunities|_|) = parse "immune to (.+)" >> extractDamageTypes
    let (|Weaknesses|_|) = parse "weak to (.+)" >> extractDamageTypes

    let (|Cohort|_|) str =
        let vals = parse "(\d+) units each with (\d+) hit points (\(.+\) )?with an attack that does (\d+) (.+) damage at initiative (\d+)" str
        
        match vals with
        | None -> None
        | Some v ->        
            let immu,weak =
                v.[3].TrimStart('(').TrimEnd(')').Split(';')
                |> Array.sort
                |> Array.fold (fun s a ->
                    match a with
                    | Immunities is -> (is,(snd s))
                    | Weaknesses ws -> ((fst s),ws)
                    | "" -> s
                    | x -> failwithf "parse error %A" x) (Set.empty,Set.empty)
            Some {
                Army = army;
                Units = v.[1] |> int;
                HitPoints = v.[2] |> int;
                Immunities = immu;
                Weaknesses = weak;
                Damage = v.[4] |> int;
                DamageType = DamageType v.[5];
                Initiative = v.[6] |> int;
            }

    let (|ImmuneSystem|_|) = parse "Immune System:"
    let (|Infection|_|) = parse "Infection:"

    match line with
    | ImmuneSystem _ -> army <- ImmuneSystem; None
    | Infection _ -> army <- Infection; None
    | Cohort c -> Some c
    | "" -> None
    | x -> failwithf "parse error %A" x

let cohorts = 
    System.IO.File.ReadLines("input/24.txt")
    |> List.ofSeq
    |> List.map parse
    |> List.choose id

let targetSelection cohorts =
    let mutable selected = Set.empty

    let targetSelect cohort =
        let targets = 
            cohorts
            |> List.filter (fun c -> c.Army <> cohort.Army && not (Set.contains c.Initiative selected))
        match targets with
        | [] -> (cohort,None)
        | ts ->
            ts
            |> List.map (fun t ->
                let dmg = (cohort.attackDamageAgainst t,t.EffectivePower,t.Initiative)
                (dmg,t))
            |> List.sortByDescending fst
            |> List.head
            |> fun x ->
                let (dmg,_,_),target = x
                match dmg with
                | i when i <= 0 -> (cohort,None)
                | _ -> 
                    selected <- selected.Add target.Initiative
                    (cohort,Some target.Initiative)

    cohorts
    |> List.map targetSelect

let mutable mc = cohorts
let attacking assaults =
    let mutable cohorts = mc |> List.map (fun c -> (c.Initiative,c)) |> Map.ofList
    let attack (c,i) =
        match i with
        | None -> ()
        | Some i ->
            let nc = cohorts.[i].attackedBy cohorts.[c.Initiative]
            cohorts <- cohorts.Add (i,nc)

    assaults
    |> List.iter attack

    cohorts
    |> Map.toList
    |> List.map snd
    |> List.filter (fun c -> c.Units > 0)


let census c a =
    c |> List.filter (fun x -> x.Army = a) |> List.sumBy (fun x -> x.Units)

while ((census mc ImmuneSystem) > 0 && (census mc Infection) > 0) do
    mc <-
        mc
        |> List.sortByDescending (fun c -> (c.Units*c.Damage,c.Initiative))
        |> targetSelection
        |> List.sortByDescending (fun (c,_) -> c.Initiative)
        |> attacking

dump "Part 1: %A" (mc |> List.sumBy (fun c -> c.Units))