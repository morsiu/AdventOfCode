type Bit =
    | Zero
    | One

module Pair =
    let map f (a, b) = (f a, f b)
    let mapBoth f g (a, b) = (f a, g b)

    let transpose x =
        let append seq a = Seq.append seq (Seq.singleton a)

        x
        |> Seq.fold (fun (a, b) (x, y) -> (append a x, append b y)) ([], [])

let toNumber (digits: seq<Bit>) =
    let weights = Seq.initInfinite (fun n -> 1 <<< n)

    digits
    |> Seq.rev
    |> Seq.fold2
        (fun number weight digit ->
            match digit with
            | One -> number + weight
            | Zero -> number)
        0
        weights

let input =
    let toBit char = if char = '0' then Zero else One

    System.IO.File.ReadLines "input.txt"
    |> Seq.map (Seq.map toBit)

let firstPart input =
    let (gamma, epsilon) =
        input
        |> Seq.transpose
        |> Seq.map (
            Seq.fold
                (fun n x ->
                    match x with
                    | One -> n + 1
                    | Zero -> n - 1)
                0
        )
        |> Seq.map (fun bias -> ((if bias > 0 then One else Zero), (if bias < 0 then One else Zero)))
        |> Pair.transpose
        |> Pair.map toNumber

    gamma * epsilon

printfn "%d" (firstPart input)
