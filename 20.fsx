let moves =
    [('N',(0, -1));
     ('E',(1, 0));
     ('S',(0, 1));
     ('W',(-1, 0))]
    |> Map.ofList

let add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

let folder state ch =
    let pos,(map:Map<(int*int),int>),stack = state
    match ch with
    | '(' -> (pos,map,pos::stack)
    | ')' -> (stack.Head,map,stack.Tail)
    | '|' -> (stack.Head,map,stack)
    | c -> 
        let here,dist = pos
        let dest = add here moves.[c]
        let edge = (dest,dist+1)
        let newMap =
            if map.ContainsKey dest && map.[dest] < dist+1
            then map
            else map.Add edge
        (edge,newMap,stack)

let input =
    System.IO.File.ReadAllLines("input/20.txt").[0].TrimStart('^').TrimEnd('$')

input
|> Seq.fold folder (((0,0),0),Map.empty,[])
|> fun (_,m,_) -> m
|> Map.toList
|> List.sortByDescending (fun (_,d) -> d)
|> fun x -> printfn "Part 1: %A" (snd x.Head)