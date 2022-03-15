let input =
    let toDigit char =
        if char >= '0' && char <= '9' then
            byte (int (char) - int ('0'))
        else
            raise (System.Exception(sprintf "Expected a number but got %c" char))

    let lines =
        System.IO.File.ReadLines("input.txt")
        |> Seq.toArray

    let rows = Array.length lines + 2
    let columns = String.length lines[0] + 2
    let heightmap = Array2D.create<byte> rows columns 255uy

    for i in 1 .. rows - 2 do
        let row = lines[i - 1]

        for j in 1 .. columns - 2 do
            heightmap[i, j] <- toDigit row[j - 1]

    heightmap

let firstPart (heightmap: byte [,]) =
    let riskFactors =
        seq {
            for i in 1 .. Array2D.length1 heightmap - 2 do
                for j in 1 .. Array2D.length2 heightmap - 2 do
                    if heightmap[i, j] < heightmap[i - 1, j]
                       && heightmap[i, j] < heightmap[i, j - 1]
                       && heightmap[i, j] < heightmap[i, j + 1]
                       && heightmap[i, j] < heightmap[i + 1, j] then
                        yield int (heightmap[i, j]) + 1
        }

    riskFactors |> Seq.sum

printfn "%d" (firstPart input)
