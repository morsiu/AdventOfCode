type Either<'x, 'y> =
    | Left of left: 'x
    | Right of right: 'y

module Bingo =
    type Board =
        { rows: int list list
          columns: int list list
          unmarked: int Set }

    type Score = Score of value: int

    let board rows : Board =
        { rows = rows |> Seq.map Seq.toList |> Seq.toList
          columns =
            rows
            |> Seq.transpose
            |> Seq.map Seq.toList
            |> Seq.toList
          unmarked = rows |> Seq.concat |> set }

    let hasBingo board =
        let hasBingo rowsOrColumns =
            rowsOrColumns |> List.exists List.isEmpty

        hasBingo board.rows || hasBingo board.columns

    let score number board =
        Score
        <| number * (board.rows |> Seq.concat |> Seq.sum)

    let mark number board =
        let removeNumber rowsOrColumns =
            rowsOrColumns
            |> List.map (fun row ->
                match List.tryFindIndex (fun x -> x = number) row with
                | Some i -> List.removeAt i row
                | _ -> row)

        if Set.contains number board.unmarked then
            let rows' = removeNumber board.rows
            let columns' = removeNumber board.columns
            let unmarked' = board.unmarked.Remove number

            { Board.rows = rows'
              columns = columns'
              unmarked = unmarked' }
        else
            board

let input () =
    let toNumbers (separator: string) (line: string) =
        line.Split(separator, System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.toArray

    let file = System.IO.File.OpenText "input.txt"
    let numbers = file.ReadLine() |> toNumbers ","

    let boards =
        seq {
            while not file.EndOfStream do
                let line = file.ReadLine()

                if not (System.String.IsNullOrWhiteSpace line) then
                    yield toNumbers " " line
        }
        |> Seq.chunkBySize 5
        |> Seq.map Bingo.board
        |> Seq.toArray

    (numbers, boards)

let firstPart numbers boards =
    let game =
        numbers
        |> Seq.fold
            (fun game number ->
                match game with
                | Left _ -> game
                | Right boards ->
                    let (boards', score) =
                        boards
                        |> Seq.mapFold
                            (fun score board ->
                                match score with
                                | None ->
                                    let board' = Bingo.mark number board

                                    if Bingo.hasBingo board' then
                                        let score' = Bingo.score number board'
                                        board', Some score'
                                    else
                                        board', None
                                | _ -> board, score)
                            None

                    match score with
                    | Some score -> Left score
                    | _ -> Right boards')
            (Right boards)

    match game with
    | Right _ -> 0
    | Left (Bingo.Score score) -> score

let (numbers, boards) = input ()

printfn "%d" (firstPart numbers boards)
