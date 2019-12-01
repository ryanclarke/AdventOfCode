let rawNumbers =
    System.IO.File.ReadLines("input/8.txt")
    |> Seq.exactlyOne
    |> fun x -> x.Split ' '
    |> Seq.map int
    |> Seq.toArray

let mutable rawIndex = -1
let nextNumber _ =
    rawIndex <- rawIndex + 1
    rawNumbers.[rawIndex]

let times i =
    List.init i id

type Node = {
    Children: Node list option
    Metadata: int list
}

let rec makeNode (_:obj) : Node =
    let childrenCount = nextNumber()
    let metadataCount = nextNumber()
    let children =
        match childrenCount with
        | 0 -> None
        | x -> x |> times |> List.map makeNode |> Some
    let metadata = metadataCount |> times |> List.map nextNumber
    {Children = children; Metadata = metadata}

let rec sumMetadata node =
    List.sum node.Metadata
  + match node.Children with
    | Some c -> c |> List.sumBy sumMetadata
    | None -> 0

let rec calculateValue node =
    match node.Children with
    | Some c ->
        node.Metadata
        |> List.filter (fun i -> i > 0 && i <= c.Length)
        |> List.sumBy (fun i -> calculateValue c.[i-1])
    | None -> 
        List.sum node.Metadata

let rootNode = makeNode()

rootNode
|> sumMetadata
|> printfn "Part 1: Sum of metadata is %A"

rootNode
|> calculateValue
|> printfn "Part 2: Value of root node is %A"