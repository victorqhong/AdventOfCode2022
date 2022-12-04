open System

let parseAssignmentPairs(assignmentPairs: string) : int * int * int * int =
    let assignments = assignmentPairs.Split(',')

    let assignment1 = assignments.[0].Split('-')
    let min1 = int(assignment1.[0])
    let max1 = int(assignment1.[1])

    let assignment2 = assignments.[1].Split('-')
    let min2 = int(assignment2.[0])
    let max2 = int(assignment2.[1])

    (min1, max1, min2, max2)

module Part1 =
    let private processItem(assignmentPairs: string) : int =
        let (min1, max1, min2, max2) = parseAssignmentPairs(assignmentPairs)
        
        let min = Math.Min(min1, min2)
        let max = Math.Max(max1, max2)

        if (min1 = min && max1 = max) || (min2 = min && max2 = max) then
            1
        else
            0

    let countFullyContainedPairs(input: string[]) : int =
        input |> Array.sumBy processItem

module Part2 =
    let private processItem(assignmentPairs: string) : int =
        let (min1, max1, min2, max2) = parseAssignmentPairs(assignmentPairs)
        if max1 < min2 || min1 > max2 then
            0
        else
            1

    let countOverlapRanges(input: string[]) : int =
        input |> Array.sumBy processItem

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt");

    let count1 = Part1.countFullyContainedPairs(input)
    printfn $"Assignment pairs where one range fully contains the other: {count1}"

    let count2 = Part2.countOverlapRanges(input)
    printfn $"Assignment pairs where there is overlap: {count2}"

    0
