open System.Drawing
let sw = System.Diagnostics.Stopwatch.StartNew()
let dump s o = printf "[%A] " sw.Elapsed; printfn s o
let ts s f = printf "[%A] %s" sw.Elapsed s; f

type Bot = {
    Radius: int
    Coord: int*int*int
}

let parse input =
    let pattern = "pos=<(?<coord>.+)>, r=(?<radius>\d+)"
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    { Radius = m.Groups.["radius"].Value |> int
      Coord = m.Groups.["coord"].Value.Split(',') |> Array.map int |> fun x -> (x.[0],x.[1],x.[2])
    }

let inRangeOf b1 b2 =
    let x1,y1,z1 = b1.Coord
    let x2,y2,z2 = b2.Coord
    let manhattan = (abs (x1-x2)) + (abs (y1-y2)) + (abs (z1-z2))
    manhattan <= b1.Radius
    
let bots = 
    System.IO.File.ReadLines("input/23jsp.txt")
    |> Seq.map parse

let strongestBot =
    bots
    |> Seq.maxBy (fun b -> b.Radius)

bots
|> Seq.filter (inRangeOf strongestBot)
|> Seq.length
|> printfn "Part 1: %A"   

// // ============================= petertseng

let nbs =
    bots |> Array.ofSeq

type Box = {
    Min: int
    Max: int
    MinIdx: int
    MaxIdx: int
    Sizes: int[]
    Size: int
    Point: (int*int*int) option
}
with
    member this.TouchedBy b4d =


let createBox coords =
    min max minIdx maxIdx = coords
    {
        Min = coords |> Array.;
        Max = max;
        MinIdx = minIdx;
        MaxIdx = maxIdx;
        Sizes = maxIdx |> Array.zip minIdx
    }

let to3d (a,b,c,d) =
    let odd i =
        i%2 = 1
    if odd (a + d) || odd (c - d) || odd (a - c)
    then None
    else
        let x = (a + d) / 2
        let y = (c - d) / 2
        let z = (a - c) / 2
        if x - y + z <> b
        then None
        else Some (x,y,z)

let aabb b =
    let to4d (x,y,z) =
        [|x + y + z; x - y + z; x + y - z; x - y - z|]
    let mins = b.Coord |> to4d |> Array.map ((-) b.Radius)
    let maxs = b.Coord |> to4d |> Array.map ((+) b.Radius)
    mins |> Array.zip maxs



let coords (b4d:int[][]) =
    [|0;1;2;3|]
    |> Array.collect (fun i ->
        b4d
        |> Array.map (fun b ->
            b.[i])
        |> Array.distinct
        |> Array.sort)

let mostIntersected b4d =
    let coords = coords b4d
    let start 







// ================================================ Attempt Jacob's formula
// let rec comb n l = 
//     match n, l with
//     | 0, _ -> [[]]
//     | _, [] -> []
//     | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let distance (ax,ay,az) (bx,by,bz) =
    abs (ax-bx) + abs (ay-by) + abs (az-bz)

let determinant a b c d e f g h i =
    a * (e * i - f * h)
  - b * (d * i - f * g)
  + c * (d * h - e * g)

let intersectionPoint (aa,ab,ac,ad) (ba,bb,bc,bd) (ca,cb,cc,cd) =
    let det = determinant aa ab ac ba bb bc ca cb cc
    if det = 0 
    then None
    else
        let x = (determinant ad ab ac bd bb bc cd cb cc) / det
        let y = (determinant aa ad ac ba bd bc ca cd cc) / det
        let z = (determinant aa ab ad ba bb bd ca cb cd) / det
        Some (x,y,z)

type Nanobot = {
    Id: int
    Center: int*int*int
    Radius: int
    Planes: (int*int*int*int) list
    Neighbors: Set<int>
}

let createNanobot bot =
    let center = bot.Coord
    let x,y,z = center
    let radius = bot.Radius
    let planes =
        [-1 .. 1] |> List.collect (fun a -> 
            [-1 .. 1] |> List.collect (fun b -> 
                [-1 .. 1] |> List.map (fun c ->
                    let d = a*x + b*y + c*z + radius
                    (a,b,c,d))))
    {
        Id = hash (center,radius)
        Center = center;
        Radius = radius;
        Planes = planes;
        Neighbors = Set.empty
    }

let intersects a b =
    (distance a.Center b.Center) <= (a.Radius + b.Radius)

let nanobotContains point nanobot =
    (distance nanobot.Center point) <= nanobot.Radius

let maximalCliques r p x (all:Map<int,Nanobot>) =
    let mutable mcs = Set.empty
    let rec bronKerbosch r p x =
        if (Set.isEmpty p) && (Set.isEmpty x)
        then mcs <- mcs.Add r
        else
            let u = Set.union p x |> Seq.maxBy (fun n -> n.Neighbors.Count)
            let mutable mp = p
            let mutable mx = x
            p
            |> Set.filter (fun v -> u.Neighbors.Contains v.Id)
            |> Set.iter (fun v ->
                let ns = v.Neighbors |> Set.map (fun n -> all.[n])
                bronKerbosch
                    (Set.ofList [v] |> Set.union r)
                    (Set.intersect mp ns)
                    (Set.intersect mx ns)
                mp <- mp.Remove v
                mx <- mx.Add v)
    bronKerbosch r p x
    mcs

let maximalCliquesLoop p1 (all:Map<int,Nanobot>) =
    let mutable mcs = Set.empty
    let mutable s = []
    s <- (Set.empty,p1,Set.empty)::s
    while not s.IsEmpty do
        let r,p,x = s.Head
        s <- s.Tail
        if (Set.isEmpty p) && (Set.isEmpty x)
        then mcs <- mcs.Add r
        else
            if not (Set.isEmpty p)
            then
                let u = Set.union p x |> Seq.maxBy (fun n -> n.Neighbors.Count)
                p
                |> Set.filter (fun v -> u.Neighbors.Contains v.Id)
                |> Set.iter (fun v ->
                    let ns = v.Neighbors |> Set.map (fun n -> all.[n])
                    s <- (r,p.Remove v,x.Add v)::s
                    s <- (Set.singleton v |> Set.union r,Set.intersect p ns,Set.intersect x ns)::s)
    mcs

// let rec bronKerbosch R P X (all:Map<int,Nanobot>) =
//     seq {
//         if (Set.isEmpty P) && (Set.isEmpty X) then
//           yield (Set.toSeq R)
//         let vPX =
//             Seq.unfold
//                 (function
//                 | (v::tailP as P, currX) ->
//                     let newX = Set.add v currX
//                     Some((v, set <| P, currX), (tailP, newX))
//                 | ([], _) -> None)
//                 (p |> Set.toList, x)
//         for (v, P, X) in vPX do
//             let n = v.Neighbors |> Set.map (fun i -> all.[i])
//             yield! bronKerbosch (Set.add v R) (Set.intersect P n) (Set.intersect X n) all
//     }

let calculateNeighbors nanobots =
    let neighbors =
        nanobots
        |> List.collect (fun a ->
            nanobots |> List.collect (fun b ->
                if intersects a b
                then [(a.Id,b.Id);(b.Id,a.Id)]
                else []))
        |> List.groupBy fst
        |> List.map (fun (k,v) -> (k,(v |> List.map snd |> Set.ofList)))
        |> Map.ofList
    nanobots
    |> List.map (fun n -> 
        if neighbors.ContainsKey n.Id
        then {n with Neighbors = neighbors.[n.Id]}
        else n)

let findBiggestIntersectionSets nanobots =
    let nbSet = nanobots |> Set.ofList
    let nbMap = nanobots |> List.map (fun n -> (n.Id,n)) |> Map.ofList
    let maximalCliques =
        maximalCliquesLoop nbSet nbMap
        |> Seq.toList
        |> List.map (Seq.toList)
    let maxCliqueSize =
        maximalCliques
        |> List.maxBy (fun c -> c.Length)
        |> List.length
    maximalCliques
    |> List.filter (fun c -> c.Length = maxCliqueSize)
let getBoundingPlanes nanobots =
    nanobots |> List.map (fun n ->
        n.Planes |> List.minBy (fun (_,_,_,d) -> d))

let findIntersectionPoints nanobots planes =
    comb 3 planes
    |> List.choose 
        (function
        | [a;b;c] -> 
            match intersectionPoint a b c with
            | None -> None
            | Some p ->
                if nanobots |> List.tryFind (nanobotContains p >> not) |> Option.isSome
                then None
                else Some p
        | _ -> failwith "comb count error" )

let findClosestPointToOrigin points =
    points |> List.minBy (fun p -> distance p (0,0,0))

let findBestPoint nanobots =
    nanobots
    |> ts "a" findBiggestIntersectionSets
    |> List.map (fun ns ->
        ns
        |> ts "b" getBoundingPlanes
        |> ts "c" findIntersectionPoints ns
        |> ts "d" findClosestPointToOrigin)
    |> ts "e" findClosestPointToOrigin
    |> fun bp ->
        (bp,(distance bp (0,0,0)))


let nanobots =
    bots
    |> List.ofSeq
    |> List.map createNanobot
    |> calculateNeighbors

//dump "%A" (findBestPoint nanobots)













// ======================================= Attempt

let clipsWith cube b =
    let x,y,z = b.Coord
    let x1,y1,z1,x2,y2,z2 = cube
    let dist = 
        (max (x1 - x) (x - x2))
      + (max (y1 - y) (y - y2))
      + (max (z1 - z) (z - z2))
    dist <= b.Radius

let bx = bots |> Seq.map (fun b -> let x,_,_ = b.Coord in x)
let by = bots |> Seq.map (fun b -> let _,y,_ = b.Coord in y)
let bz = bots |> Seq.map (fun b -> let _,_,z = b.Coord in z)
let maxX = bx |> Seq.max
let minX = bx |> Seq.min
let maxY = by |> Seq.max
let minY = by |> Seq.min
let maxZ = bz |> Seq.max
let minZ = bz |> Seq.min
let mutable s =
    let max = [(maxX-minX);(maxY-minY);(maxZ-minZ)] |> Seq.max
    Seq.initInfinite (fun i -> pown 2 i)
    |> Seq.find (fun i -> i >= max)

let mutable q = []

let cadd (x1,y1,z1) =
    let x2 = x1 + s - 1
    let y2 = y1 + s - 1
    let z2 = z1 + s - 1
    let inRange =
        bots
        |> Seq.filter (clipsWith (x1, y1, z1, x2, y2, z2))
        |> Seq.length
    if inRange > 0
    then
        let dist =
            (min (abs x1) (abs x2))
          + (min (abs y1) (abs y2))
          + (min (abs z1) (abs z2))
        q <- (-inRange, dist, s, x1, y1, z1)::q
//976, 51429369
cadd (minX,minY,minZ)
while not q.IsEmpty do
    let q1 = q |> List.sort
    let score,dist,s1,x,y,z = q1.Head

    if s = 1
    then
        q1 |> List.take 5 |> List.iter (printfn "%A")
        printfn "best location: %A" (x,y,z)
        printfn "bots in range: %A" -score
        printfn "distance from origin: %A" dist

        nanobots
        |> List.filter (nanobotContains (x,y,z))
        |> (fun ns ->
            ns
            |> ts "b" getBoundingPlanes
            |> ts "c" findIntersectionPoints ns
            |> ts "d" findClosestPointToOrigin)
        //|> ts "e" findClosestPointToOrigin
        |> fun bp ->
            (bp,(distance bp (0,0,0)))
        |> dump "%A"


        exit 0
    else
        q <- q1.Tail
        s <- s1/2
        cadd (x, y, z)
        [s]//; s/2]
        |> List.filter (fun j -> j<>0)
        |> List.distinct
        |> List.iter (fun v ->
            cadd (x, y, z+v)
            cadd (x, y+v, z)
            cadd (x, y+v, z+v)
            cadd (x+v, y, z)
            cadd (x+v, y, z+v)
            cadd (x+v, y+v, z)
            cadd (x+v, y+v, z+v))



// ============= Attempt

// let isInRange coord b =
//     Array.zip coord b.Coord
//     |> Array.sumBy (fun (i,j) -> abs (i-j))
//     <= b.Radius

// let rec run (c:int[]) oom =
//     let pow = pown 10 oom
//     let step = if pow > 1 then pow/7 else 1
//     let x = c.[0]
//     let y = c.[1]
//     let z = c.[2]

//     //dump "Starting %A" oom
//     seq { z-pow .. step .. z+pow } |> Seq.collect (fun z ->
//         seq { y-pow .. step .. y+pow } |> Seq.collect (fun y ->
//             seq { x-pow .. step .. x+pow } |> Seq.map (fun x -> [|x; y; z|])))
//     |> Seq.map (fun coord ->
//         (coord,
//             bots
//             |> Seq.filter (isInRange coord)
//             |> Seq.length))
//     |> Seq.sortByDescending snd
//     |> Seq.take 1
//     |> Seq.map (fun (c,l) ->
//         // dump "Max of %A: %A - %A" oom c l
//         if oom = 1
//         then
//             let dist = (Array.zip c [|0;0;0|] |> Array.sumBy (fun (i,j) -> abs (i-j)))
//             dump "finished %d: %A %A %A" oom c l dist
//             dist
//         else run c (oom-1))
//     |> Seq.min

// run [|0;0;0|] 8 |> dump "Part 2: %A"