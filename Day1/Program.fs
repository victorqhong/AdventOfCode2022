open System

let private makePartitions (array: string[]) =
    let index = array |> Array.tryFindIndex(fun x -> x = "")
    match index with
        | Some(value) -> array |> Array.splitAt value
        | None -> (array, [||])

let private convertAndSum (array: string[]) =
    array |> Array.sumBy Convert.ToInt32

module Part1 =
    let rec private findGreatestSumRec (x: string[] * string[], sum: string[] -> int) = 
        match x with
        | (a, [||]) -> sum(a)
        | (a, b) -> Math.Max(sum(a), findGreatestSumRec(makePartitions(b.[1..]), sum))

    let findGreatestSum (values: string[]) =
        findGreatestSumRec(makePartitions(values), convertAndSum)

module Part2 =
    let rec private calculateElfCalories (values: string[]) : int list =
        match makePartitions(values) with
            | (a, [||]) -> [convertAndSum(a)]
            | (a, b) -> List.concat(seq { [convertAndSum(a)]; calculateElfCalories(b.[1..]) })

    let topThreeSum (values: string[]) =
        let sortedElfCalories = calculateElfCalories(values) |> List.sortDescending
        sortedElfCalories.[0] + sortedElfCalories.[1] + sortedElfCalories.[2]

[<EntryPoint>]
let main argv =
    let values = System.IO.File.ReadAllLines("input.txt");

    let greatestSum = Part1.findGreatestSum(values)
    printfn $"Greatest number of calories carried by an elf: {greatestSum}"

    let topThreeSum = Part2.topThreeSum(values)
    printfn $"Sum of top three calories carried by elfs: {topThreeSum}"

    0