open System

let makePartitions (array: string[]) =
    let index = array |> Array.tryFindIndex(fun x -> x = "")
    match index with
        | Some(value) -> array |> Array.splitAt value
        | None -> (array, [||])

let private convertAndSum (array: string[]) =
    array |> Array.sumBy Convert.ToInt32

module Part1 =
    let rec findGreatestSum (input: string[]) =
        match makePartitions(input) with
        | (a, [||]) -> convertAndSum(a)
        | (a, b) -> Math.Max(convertAndSum(a), findGreatestSum(b.[1..]))

module Part2 =
    let rec private calculateElfCalories (values: string[]) : int list =
        match makePartitions(values) with
            | (a, [||]) -> [convertAndSum(a)]
            | (a, b) -> List.concat(seq { [convertAndSum(a)]; calculateElfCalories(b.[1..]) })

    let topThreeSum (input: string[]) =
        calculateElfCalories(input) |> List.sortDescending |> List.take 3 |> List.sum

[<EntryPoint>]
let main argv =
    let values = System.IO.File.ReadAllLines("input.txt");

    let greatestSum = Part1.findGreatestSum(values)
    printfn $"Greatest number of calories carried by an elf: {greatestSum}"

    let topThreeSum = Part2.topThreeSum(values)
    printfn $"Sum of top three calories carried by elfs: {topThreeSum}"

    0