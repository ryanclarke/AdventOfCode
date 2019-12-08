#load "utils/Base.fsx"
open Base

inputFile "08" string
|> Seq.exactlyOne
|> Seq.map (string >> int)
|> Seq.chunkBySize 150
|> Seq.map (fun layer ->
    layer
    |> Array.groupBy id
    |> Map.ofArray
)
|> Seq.minBy (fun m -> m.[0].Length)
|> fun m -> m.[1].Length * m.[2].Length
|> solution "08a" 2016



