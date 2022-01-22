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

printf "%d" (firstPart input)
