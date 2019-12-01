open System
let sw = System.Diagnostics.Stopwatch.StartNew()

let dump (format:Printf.TextWriterFormat<'T>) : 'T =
    printf "[%dms] " sw.ElapsedMilliseconds; printfn format

let square (x,y) size =
    let l = Math.Max(y-1, 0)
    let r = Math.Min(y+1, size-1)
    let t = Math.Max(x-1, 0)
    let b = Math.Min(x+1, size-1)
    [|l .. r|] |> Array.collect (fun y2 ->
        [|t .. b|] |> Array.map (fun x2 -> (x2,y2)))
    |> Array.except [|(x,y)|]

let neighboorhood (acres:char[][]) coords =
    coords
    |> Array.map (fun (x,y) -> acres.[y].[x])
    |> Array.countBy id
    |> Map.ofArray

let folder (state:int*(Map<int,int>)*char[][]) _ =
    let min,prev,acres = state
    acres |> Array.mapi (fun y row -> 
        row |> Array.mapi (fun x acre ->
            let adj =
                square (x,y) acres.Length
                |> neighboorhood acres
            let has i c = adj.TryFind c |> Option.defaultValue 0 >= i
            match acre with
            | '.' -> if has 3 '|' then '|' else '.'
            | '|' -> if has 3 '#' then '#' else '|'
            | '#' -> if has 1 '#' && has 1 '|' then '#' else '.'
            | _ -> failwith "Invalid char"))
    |> fun a ->
        let h = hash a
        if prev.ContainsKey h
        then
            dump "%d %d" prev.[h] min
        (min+1,(prev.Add(h,min)),a)


let run (acres:string[]) times =
    let grid =
        acres |> Array.map (fun y ->
            y |> Seq.toArray)
    Seq.init times id
    |> Seq.fold folder (0,Map.empty,grid)
    |> fun (_,_,x) -> x
    |> Array.collect id
    |> Array.groupBy id 
    |> Map.ofArray
    |> fun m -> 
        (m.['|'].Length, m.['#'].Length)

let testAcres =
    [|".#.#...|#.";
      ".....#|##|";
      ".|..|...#.";
      "..|#.....#";
      "#.#|||#|#|";
      "...#.||...";
      ".|....|...";
      "||...#|.#|";
      "|.||||..|.";
      "...#.|..|."|]

let acres = System.IO.File.ReadAllLines("input/18.txt")

run testAcres 10 |> fun x -> dump "%d * %d = %d" (fst x) (snd x) (fst x * snd x)
run acres 10 |> fun x -> dump "%d * %d = %d" (fst x) (snd x) (fst x * snd x)
run acres (479+17) |> fun x -> dump "%d * %d = %d" (fst x) (snd x) (fst x * snd x)