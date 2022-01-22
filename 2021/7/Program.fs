let input =
    System.IO.File.ReadLines "input.txt"
    |> Seq.map (fun x -> x.Split ',')
    |> Seq.concat
    |> Seq.map int

let firstPart input =
    let sorted = input |> Seq.toArray |> Array.sort

    let position =
        if sorted.Length % 2 = 1 then
            sorted[sorted.Length / 2]
        else
            (sorted[sorted.Length / 2]
             + sorted[sorted.Length / 2 + 1])
            / 2

    sorted |> Seq.sumBy (fun x -> abs (position - x))

let secondPart (input: int []) =
    let minPosition = Seq.min input
    let maxPosition = Seq.max input
    let arithmeticSeqSum n = (n + 1) * n / 2

    let moveCost from (to_: int) =
        let diff = abs (to_ - from)
        arithmeticSeqSum diff

    let positions =
        Seq.unfold
            (fun x ->
                if x <= maxPosition then
                    Some(x, x + 1)
                else
                    None)
            minPosition

    let moveCosts =
        positions
        |> Seq.map (fun x -> input |> Seq.sumBy (moveCost x))

    Seq.min moveCosts

printf "%d" (secondPart (input |> Seq.toArray))
