open System
#load "utils/Base.fsx"
open Base
    
type Asteroid = {
    Coord: int*int
    Angle: float
    Range: float
}

let toCoords input =
    input |> Seq.toList |> List.mapi (fun y line ->
        line |> Seq.toList |> List.mapi (fun x marking ->
            match marking with
            | '#' -> Some (x,y)
            | _ -> None
        )
    )
    |> List.collect id
    |> List.choose id

let asteroidInfo (sx,sy) (ax,ay) =
    let x = float (sx - ax)
    let y = float (sy - ay)
    let hypotenuse = Math.Sqrt(Math.Pow(x,2.0) + Math.Pow(y,2.0))
    let angle =
        match Math.Atan2(float y, float x) with
        | a when a < Math.PI/2.0 -> a + 2.0 * Math.PI 
        | a -> a

    match x,y with
    | 0.0,0.0 -> None
    | _ -> Some {Coord = (ax,ay); Angle = angle; Range = hypotenuse}

let getBestStationWithAstroids coords =
    coords |> List.map (fun station ->
        coords |> List.map (asteroidInfo station)
        |> List.choose id
        |> fun astroids ->
            let visibleAstroids = astroids |> List.distinctBy (fun a -> a.Angle) |> List.length
            station,visibleAstroids,astroids
    )
    |> List.maxBy (fun (_,visible,_) -> visible)


// ===================================================================================================================
// 2019.10 A
// ===================================================================================================================

let solveA (input:seq<string>) =
    let station,visibleAstroids,_ = input |> toCoords |> getBestStationWithAstroids
    station,visibleAstroids

inputFile "10a1" string |> solveA |> solution "10a.1" ((5,8),33)
inputFile "10a2" string |> solveA |> solution "10a.2" ((1,2),35)
inputFile "10a3" string |> solveA |> solution "10a.3" ((6,3),41)
inputFile "10a4" string |> solveA |> solution "10a.4" ((11,13),210)
inputFile "10" string |> solveA |> solution "10a" ((29,28),256)

// ===================================================================================================================
// 2019.10 B
// ===================================================================================================================

let solveB place (input:seq<string>) =
    let _,_,asteroids = input |> toCoords |> getBestStationWithAstroids

    let sortedAsteroids =
        asteroids
        |> List.groupBy (fun a -> a.Angle)
        |> List.sortBy fst
        |> List.map (fun (angle,asteroid) ->
            let sorted = asteroid |> List.sortBy (fun a -> a.Range)
            (angle,sorted)
        )
    
    let mostInline =
        sortedAsteroids
        |> List.map (snd >> List.length)
        |> List.max
    
    [ 0 .. mostInline-1 ]
    |> List.collect (fun rotation ->
        sortedAsteroids
        |> List.map (fun (_,asteroids) ->
            match asteroids.Length with
            | l when l > rotation ->
                Some asteroids.[rotation]
            | _ -> None
        )
    )
    |> List.choose id
    |> List.map (fun a -> a.Coord)
    |> fun a ->
        let x,y = a.[place-1]
        x * 100 + y

inputFile "10a4" string |> solveB 1 |> solution "10b.1" 1112
inputFile "10a4" string |> solveB 2 |> solution "10b.2" 1201
inputFile "10a4" string |> solveB 3 |> solution "10b.3" 1202
inputFile "10a4" string |> solveB 10 |> solution "10b.4" 1208
inputFile "10a4" string |> solveB 20 |> solution "10b.5" 1600
inputFile "10a4" string |> solveB 50 |> solution "10b.6" 1609
inputFile "10a4" string |> solveB 100 |> solution "10b.7" 1016
inputFile "10a4" string |> solveB 199 |> solution "10b.8" 906
inputFile "10a4" string |> solveB 200 |> solution "10b.9" 802
inputFile "10a4" string |> solveB 201 |> solution "10b.10" 1009
inputFile "10a4" string |> solveB 299 |> solution "10b.11" 1101
inputFile "10" string |> solveB 200 |> solution "10b" 1707
