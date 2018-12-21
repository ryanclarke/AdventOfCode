let moves =
    [('N',(0, -1));
     ('E',(1, 0));
     ('S',(0, 1));
     ('W',(-1, 0))]
    |> Map.ofList

let add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

let folder state c =
    let pos,(map:Map<(int*int),int>),stack = state
    match c with
    | '(' -> (pos,map,pos::stack)
    | ')' -> (stack.Head,map,stack.Tail)
    | '|' -> (stack.Head,map,stack)
    | _ -> 
        let here,dist = pos
        let dest = add here moves.[c]
        let edge = (dest,dist+1)
        let newMap =
            if map.ContainsKey dest && map.[dest] < dist+1
            then map
            else map.Add edge
        (edge,newMap,stack)

System.IO.File.ReadAllLines("input/20.txt").[0].TrimStart('^').TrimEnd('$')
|> Seq.fold folder (((0,0),0),Map.empty,[])
|> fun (_,m,_) -> m
|> Map.toList
|> List.sortByDescending (fun (_,d) -> d)
|> List.map snd
|> fun x ->
    printfn "Part 1: %A" x.Head
    printfn "Part 2: %A" (x |> List.filter (fun a -> a >= 1000) |> List.length)