open System

let mapItemPriority(item: char) : int =
    let value = int(item)
    let uppercase = 65 <= value && value <= 90
    let lowercase = 97 <= value && value <= 122
    match (uppercase, lowercase) with
        | (true, false) -> value - 65 + 27
        | (false, true) -> value - 96
        | (_, _) -> raise(Exception())

module Part1 =
    let private compartmentItemsFirst(items: string) : string =
        items.Substring(0, items.Length / 2)

    let private compartmentItemsSecond(items: string) : string =
        items.Substring(items.Length / 2, items.Length / 2)

    let private findCommonItem(firstItems: string, secondItems: string) : char =
        let hashmap : bool[] = Array.zeroCreate 52
        Array.ForEach(firstItems.ToCharArray(), fun x -> hashmap.[mapItemPriority(x) - 1] <- true)
        let duplicates = secondItems.ToCharArray() |> Array.filter(fun x -> hashmap.[mapItemPriority(x) - 1]) |> Array.distinct
        match duplicates.Length with
            | 1 -> duplicates.[0]
            | _ -> raise(Exception())

    let private processItem(item: string) : int =
        let firstItems = compartmentItemsFirst(item)
        let secondItems = compartmentItemsSecond(item)
        let commonItem = findCommonItem(firstItems, secondItems)
        mapItemPriority(commonItem)

    let calculatePrioritySum(input: string[]) : int =
        input |> Array.sumBy(processItem)

module Part2 =
    let private calculateGroupPriority(group: string[]) : int =
        let bitmap1 : bool[] = Array.zeroCreate 52
        let bitmap2 : bool[] = Array.zeroCreate 52
        let bitmap3 : bool[] = Array.zeroCreate 52
        Array.ForEach(group.[0].ToCharArray(), fun x -> bitmap1.[mapItemPriority(x) - 1] <- true)
        Array.ForEach(group.[1].ToCharArray(), fun x -> bitmap2.[mapItemPriority(x) - 1] <- true)
        Array.ForEach(group.[2].ToCharArray(), fun x -> bitmap3.[mapItemPriority(x) - 1] <- true)
        let result = [| 0 .. 51 |] |> Array.map(fun x -> bitmap1.[x] && bitmap2.[x] && bitmap3.[x])
        let index = result |> Array.findIndex(fun x -> x)
        index + 1

    let calculatePrioritySum(input: string[]) : int =
        let groups = input |> Array.splitInto(input.Length / 3)
        groups |> Array.sumBy(fun x -> calculateGroupPriority(x))

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt");

    let sumPriority1 = Part1.calculatePrioritySum(input)
    printfn $"Sum of the part 1 priorities: {sumPriority1}"

    let sumPriority2 = Part2.calculatePrioritySum(input)
    printfn $"Sum of the part 2 priorities: {sumPriority2}"

    0
