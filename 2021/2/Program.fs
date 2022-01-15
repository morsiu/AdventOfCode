type Command =
    | Up of n: int
    | Down of n: int
    | Forward of n: int

let toCommand (line: string) =
    let toNumber (string: string) =
        match System.Int32.TryParse string with
        | true, number -> Some number
        | _ -> None

    let toCommand (string: string) (number: int) =
        match string with
        | "up" -> Up number |> Some
        | "down" -> Down number |> Some
        | "forward" -> Forward number |> Some
        | _ -> None

    let parts =
        match line.Split ' ' with
        | [| operation; number |] -> Some(operation, number)
        | _ -> None

    let command =
        match parts with
        | Some (command, number) ->
            number
            |> toNumber
            |> Option.bind (toCommand command)
        | _ -> None

    command

let input =
    System.IO.File.ReadLines "input.txt"
    |> Seq.choose toCommand
    
let (position, depth) =
    input
    |> Seq.fold 
        (fun (x, y) cmd ->
            match cmd with 
            | Up n -> (x, y - n)
            | Down n -> (x, y + n)
            | Forward n -> (x + n, y))
        (0, 0)

printfn "%d" (position * depth)
