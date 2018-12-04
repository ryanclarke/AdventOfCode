open System
open System.Text.RegularExpressions

let parse pattern input =
    let regex = sprintf "^\[(?<datetime>[0-9 :-]+)\] (?<act>%s)$" pattern
    let m = Regex.Match(input, regex)
    if (m.Success) then Some (m.Groups) else None

let (|BeginShiftLog|_|) log = parse "Guard #(?<guard>\d+) begins shift" log
let (|AsleepLog|_|) log = parse "falls asleep" log
let (|AwakeLog|_|) log = parse "wakes up" log

type Msg =
    | BeginShift of int
    | Asleep of DateTime
    | Awake of DateTime
    | FAIL

let toMsg log =
    match log with
    | BeginShiftLog x -> BeginShift (int x.["guard"].Value)
    | AsleepLog x -> Asleep (DateTime.Parse(x.["datetime"].Value))
    | AwakeLog x -> Awake (DateTime.Parse(x.["datetime"].Value))
    | _ -> FAIL

type State = {
    CurrentGuard: int
    LastAsleep: int
    GuardMinutes: Map<int, int[]>
}
let emptyState = {CurrentGuard = 0; LastAsleep = 0; GuardMinutes = Map.empty}

let minutesSleptRecorder (state:State) (msg:Msg) =
    match msg with
    | BeginShift i ->
        { state with 
            CurrentGuard = i; 
            GuardMinutes =
                if (state.GuardMinutes |> Map.containsKey i) then
                    state.GuardMinutes
                else
                    state.GuardMinutes.Add (i, Array.zeroCreate(59)) }
    | Asleep dt -> 
        { state with
            LastAsleep = dt.Minute }
    | Awake dt ->
        let tup =
            let mins = seq { state.LastAsleep .. dt.Minute-1 }
            state.GuardMinutes.[state.CurrentGuard]
            |> Array.mapi (fun i x ->
                if (mins |> Seq.contains i) then
                    x + 1
                else
                    x)
        { state with
            GuardMinutes = 
                state.GuardMinutes.Add (state.CurrentGuard, tup) }
    | FAIL -> 
        state

let guardMinutes =
    System.IO.File.ReadLines("input/4.txt")
    |> Seq.sort
    |> Seq.map toMsg
    |> Seq.fold minutesSleptRecorder emptyState
    |> fun x -> x.GuardMinutes

let strategy number func =
    guardMinutes
    |> Seq.maxBy (fun x -> x.Value |> func)
    |> fun x -> (x.Key * (Array.IndexOf(x.Value, (Array.max x.Value))))
    |> printfn "Strategy %d : %A" number

strategy 1 Array.sum
strategy 2 Array.max