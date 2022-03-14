[<Struct>]
type Signal =
    | A
    | B
    | C
    | D
    | E
    | F
    | G

type Segment =
    | T = 0
    | TL = 1
    | BL = 2
    | B = 3
    | TR = 4
    | BR = 5
    | M = 6

[<System.Flags>]
type Segments =
    | None = 0
    | T = 1
    | TL = 2
    | BL = 4
    | B = 8
    | TR = 16
    | BR = 32
    | M = 64
    | Zero = 63
    | One = 48
    | Two = 93
    | Three = 121
    | Four = 114
    | Five = 107
    | Six = 111
    | Seven = 49
    | Eight = 127
    | Nine = 123

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

let secondPart entries =
    let permutations array =
        let swap (a: 'a []) x y =
            let ax = a[x]
            a[x] <- a[y]
            a[y] <- ax

        // Adapted 'procedure generate' from https://en.wikipedia.org/wiki/Heap%27s_algorithm
        let rec permutations' k array =
            seq {
                if k = 1 then
                    yield (Array.copy array)
                else
                    yield! permutations' (k - 1) array

                    for i in 0 .. (k - 2) do
                        if k % 2 = 0 then
                            swap array i (k - 1)
                        else
                            swap array 0 (k - 1)

                        yield! permutations' (k - 1) array
            }

        permutations' (Array.length array) (Array.copy array)

    let toSegment (signals: Signal []) signal : Segment =
        let index = Array.findIndex (fun x -> x = signal) signals
        LanguagePrimitives.EnumOfValue(index)

    let toSegmentFlags (segment: Segment) : Segments =
        let shift = LanguagePrimitives.EnumToValue segment
        LanguagePrimitives.EnumOfValue(1 <<< shift)

    let toSegments (toSegment: Signal -> Segment) (signals: SignalSet) =
        signals
        |> Seq.map toSegment
        |> Seq.fold (fun a b -> a ||| (toSegmentFlags b)) Segments.None

    let toDigit segments =
        match segments with
        | Segments.Zero -> 0
        | Segments.One -> 1
        | Segments.Two -> 2
        | Segments.Three -> 3
        | Segments.Four -> 4
        | Segments.Five -> 5
        | Segments.Six -> 6
        | Segments.Seven -> 7
        | Segments.Eight -> 8
        | Segments.Nine -> 9
        | _ -> raise (System.InvalidOperationException(sprintf "Segments %O do not represent a digit" segments))

    let toNumber digits =
        Seq.foldBack (fun digit (multiplier, number) -> (multiplier * 10, number + (digit * multiplier))) digits (1, 0)
        |> snd

    let signals = [| A; B; C; D; E; F; G |]

    let allToSegments =
        permutations signals
        |> Seq.map (toSegment >> toSegments)
        |> Seq.toArray

    let digits =
        Set.ofArray [| Segments.Zero
                       Segments.One
                       Segments.Two
                       Segments.Three
                       Segments.Four
                       Segments.Five
                       Segments.Six
                       Segments.Seven
                       Segments.Eight
                       Segments.Nine |]

    entries
    |> Seq.map (fun entry ->
        let validToSegment =
            allToSegments
            |> Seq.filter (fun x -> (entry.patterns |> Seq.map x |> Set.ofSeq) = digits)
            |> Seq.tryExactlyOne

        match validToSegment with
        | Some a -> entry.output |> Seq.map (a >> toDigit) |> toNumber
        | None -> raise (System.InvalidOperationException(sprintf "Failed to resolve entry %O" entry)))
    |> Seq.sum

printfn "%d" (secondPart input)
