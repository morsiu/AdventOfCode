[<Struct>]
type Signal =
    | A
    | B
    | C
    | D
    | E
    | F
    | G

type SignalSet = Set<Signal>

type Entry =
    { patterns: seq<SignalSet>
      output: seq<SignalSet> }

let input =
    let toSignal (a: char) =
        match a with
        | 'a' -> A
        | 'b' -> B
        | 'c' -> C
        | 'd' -> D
        | 'e' -> E
        | 'f' -> F
        | 'g' -> G
        | _ -> raise (System.InvalidOperationException(sprintf "Invalid signal character %c" a))

    let toSignalSet a = a |> Seq.map toSignal |> Set.ofSeq
    let lines = System.IO.File.ReadLines "input.txt"

    lines
    |> Seq.map (fun x -> x.Split('|', System.StringSplitOptions.None))
    |> Seq.map (fun a ->
        let parts =
            a
            |> Seq.map (fun x ->
                x.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                |> Seq.map toSignalSet)
            |> Seq.take 2
            |> Seq.toArray

        { patterns = parts[0]
          output = parts[1] })

let firstPart (entries: seq<Entry>) =
    entries
    |> Seq.map (fun x ->
        x.output
        |> Seq.filter (fun y ->
            match y.Count with
            | 2
            | 3
            | 4
            | 7 -> true
            | _ -> false)
        |> Seq.length)
    |> Seq.sum

printfn "%d" (firstPart input)
