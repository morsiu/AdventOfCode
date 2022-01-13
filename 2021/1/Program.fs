let firstPart input =
    input
    |> Seq.pairwise
    |> Seq.where (fun (a, b) -> a < b)
    |> Seq.length

let secondPart input =
    input
    |> Seq.windowed 3
    |> Seq.map (fun x -> Seq.sum x)
    |> firstPart

let input =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map (fun x -> int x)

printfn "%d" (secondPart input)
