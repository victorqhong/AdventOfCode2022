open System

let mapItemPriority(item: char) : int =
    let value = int(item)
    let uppercase = 65 <= value && value <= 90
    let lowercase = 97 <= value && value <= 122
    match (uppercase, lowercase) with
        | (true, false) -> value - 65 + 27
        | (false, true) -> value - 96
        | (_, _) -> raise(Exception())

let createBitmap(items: char[]) : bool[] =
    let bitmap : bool[] = Array.zeroCreate 52
    Array.ForEach(items, fun x -> bitmap.[mapItemPriority(x) - 1] <- true)
    bitmap

module Part1 =
    let private findCommonItemPriority(items: char[] * char[]) : int =
        let (firstItems, secondItems) = items
        let bitmap1 : bool[] = createBitmap firstItems
        let bitmap2 : bool[] = createBitmap secondItems
        let index = [| 0 .. 51 |] |> Array.map(fun x -> bitmap1.[x] && bitmap2.[x]) |> Array.findIndex(fun x -> x)
        index + 1

    let calculatePrioritySum(input: string[]) : int =
        input |> Array.sumBy (fun item -> item.ToCharArray() |> Array.splitAt(item.Length / 2) |> findCommonItemPriority)

module Part2 =
    let private calculateGroupPriority(group: string[]) : int =
        let bitmap1 : bool[] = createBitmap(group.[0].ToCharArray())
        let bitmap2 : bool[] = createBitmap(group.[1].ToCharArray())
        let bitmap3 : bool[] = createBitmap(group.[2].ToCharArray())
        let index = [| 0 .. 51 |] |> Array.map(fun x -> bitmap1.[x] && bitmap2.[x] && bitmap3.[x]) |> Array.findIndex(fun x -> x)
        index + 1

    let calculatePrioritySum(input: string[]) : int =
        input |> Array.splitInto(input.Length / 3) |> Array.sumBy calculateGroupPriority

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt");

    let sumPriority1 = Part1.calculatePrioritySum(input)
    printfn $"Sum of the part 1 priorities: {sumPriority1}"

    let sumPriority2 = Part2.calculatePrioritySum(input)
    printfn $"Sum of the part 2 priorities: {sumPriority2}"

    0
