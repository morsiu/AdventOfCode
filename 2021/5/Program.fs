type Line = Line of x1: int * y1: int * x2: int * y2: int

let input =
    let regex =
        new System.Text.RegularExpressions.Regex "(\d+),(\d+) -> (\d+),(\d+)"

    let toLine line =
        let x = regex.Match line

        if x.Success then
            let x1 = (int x.Groups.[1].Value)
            let y1 = (int x.Groups.[2].Value)
            let x2 = (int x.Groups.[3].Value)
            let y2 = (int x.Groups.[4].Value)

            Some <| Line(x1, y1, x2, y2)
        else
            None

    System.IO.File.ReadLines "input.txt"
    |> Seq.choose toLine
    |> Seq.toArray

let firstPart lines =
    let lines' =
        lines
        |> Seq.where (fun (Line (x1, y1, x2, y2)) -> x1 = x2 || y1 = y2)

    let plane =
        let (xmax, ymax) =
            lines'
            |> Seq.fold
                (fun (xmax, ymax) (Line (x1, y1, x2, y2)) ->
                    let xmax' = max xmax (max x1 x2)
                    let ymax' = max ymax (max y1 y2)
                    (xmax', ymax'))
                (0, 0)

        Array2D.zeroCreate (xmax + 1) (ymax + 1)

    lines'
    |> Seq.fold
        (fun overlapCount (Line (x1, y1, x2, y2)) ->
            let points =
                seq {
                    let dx = if x1 < x2 then 1 else -1
                    let dy = if y1 < y2 then 1 else -1

                    for x in x1 .. dx .. x2 do
                        for y in y1 .. dy .. y2 do
                            x, y
                }

            points
            |> Seq.fold
                (fun overlapCount (x, y) ->
                    let overlap = plane.[x, y]
                    let overlap' = overlap + 1
                    plane.[x, y] <- overlap'

                    if overlap' = 2 then
                        overlapCount + 1
                    else
                        overlapCount)
                overlapCount)
        0

printfn "%d" (firstPart input)
