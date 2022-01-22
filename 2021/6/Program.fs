module Simulation =
    type Simulation = private Simulation of int * int64 []

    let create (population : int64 []) = Simulation(0, population[ 0..8 ])
    let count (Simulation(_, population)) = Seq.sum population
    let private getPop age (Simulation(day, population)) =
        Array.get population ((day + age) % 9)
    let private setPop age count (Simulation(day, population)) =
        Array.set population ((day + age) % 9) count

    let private addPop age count simulation =
        let existingCount = getPop age simulation
        setPop age (existingCount + count) simulation

    let private takePop age simulation =
        let count = getPop age simulation
        setPop age 0 simulation
        count

    let private nextDay (Simulation(day, population)) =
        Simulation((day + 1) % 9, population)

    let simulate (simulation : Simulation) =
        let newFish = takePop 0 simulation
        let simulation' = nextDay simulation
        setPop 8 newFish simulation'
        addPop 6 newFish simulation'
        simulation'

let input =
    let population : int64 [] = Array.zeroCreate 9
    (System.IO.File.ReadLines "input.txt")
    |> Seq.map (fun x -> x.Split ',')
    |> Seq.concat
    |> Seq.map int
    |> Seq.iter (fun age ->
           let count = Array.get population age
           Array.set population age (count + 1L))
    |> ignore
    Simulation.create population

let populationAfterDays days (simulation : Simulation.Simulation) =
    let result =
        Seq.replicate days 0
        |> Seq.fold (fun simulation _ -> Simulation.simulate simulation)
               simulation
    Simulation.count result

let firstPart simulation = populationAfterDays 80 simulation
let secondPart simulation = populationAfterDays 256 simulation

printfn "%d" (secondPart input)
