#load "utils/Base.fsx"
open Base

let toColor char =
    match char with
    | '0' -> ' '
    | '1' -> '█'
    | '2' -> '#'
    | err -> failwithf "Invalid color %A" err

let input =
    inputFile "08" string
    |> Seq.exactlyOne
    |> Seq.map toColor
    |> Seq.chunkBySize 150

input
|> Seq.map (fun layer ->
    layer
    |> Array.groupBy id
    |> Map.ofArray
)
|> Seq.minBy (fun m -> m.[' '].Length)
|> fun m -> m.['█'].Length * m.['#'].Length
|> solution "08a" 2016

let solve input =
    let image = Array.create 150 '#'

    let applyPixel index pixel =
        if image.[index] = '#'
        then image.[index] <- pixel
        else ()

    let applyLayer layer =
        layer
        |> Array.iteri applyPixel

    input
    |> Seq.iter applyLayer

    image
    |> Array.chunkBySize 25
    |> Array.map cstring
    |> Array.iter (printfn "%s")
    0

solve input
|> solution "08b" 0
