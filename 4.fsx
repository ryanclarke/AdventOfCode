open System
open System.Text.RegularExpressions

let parse pattern input =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some (m.Groups.[1].Value |> int) else None

let (|BeginShift|_|) = parse "Guard #(\d+)"
let (|Asleep|_|) = parse ":(\d{2})\] falls asleep"
let (|Awake|_|) = parse ":(\d{2})\] wakes up"

type State = {
    CurrentGuard: int
    LastAsleep: int
    GuardMinutes: Map<int, int[]>
}
let emptyState = {CurrentGuard = 0; LastAsleep = 0; GuardMinutes = Map.empty}

let minutesSleptRecorder (state:State) (log:string) : State =
    match log with
    | BeginShift guardId ->
        { state with 
            CurrentGuard = guardId; 
            GuardMinutes =
                match state.GuardMinutes.TryFind guardId with
                | Some _ -> state.GuardMinutes
                | None -> state.GuardMinutes.Add (guardId, Array.zeroCreate(59)) }
    | Asleep asleepMinute -> 
        { state with
            LastAsleep = asleepMinute }
    | Awake awakeMinute ->
        let minutes =
            let napMins = seq { state.LastAsleep .. awakeMinute-1 }
            state.GuardMinutes.[state.CurrentGuard]
            |> Array.mapi (fun i x -> if (napMins |> Seq.contains i) then x + 1 else x)
        { state with
            GuardMinutes = state.GuardMinutes |> Map.add state.CurrentGuard minutes }
    | _ -> 
        state

type GuardMinutes = {
    Id: int
    TimesSleptByMinute: int[]
} with
    member this.SleepiestMinute =
        Array.IndexOf(this.TimesSleptByMinute, (Array.max this.TimesSleptByMinute))

let guardMinutes =
    System.IO.File.ReadLines("input/4.txt")
    |> Seq.sort
    |> Seq.fold minutesSleptRecorder emptyState
    |> fun x -> x.GuardMinutes 
    |> Map.toSeq
    |> Seq.map (fun x -> {Id = fst x; TimesSleptByMinute = snd x})

let strategy number func =
    guardMinutes
    |> Seq.maxBy (fun x -> x.TimesSleptByMinute |> func)
    |> fun x -> x.Id * x.SleepiestMinute
    |> printfn "Strategy %d: %A" number

strategy 1 Array.sum
strategy 2 Array.max