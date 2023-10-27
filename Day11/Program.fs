open System

type Monkey(definition: string[]) =
    let startingItems = definition.[1].Split(':').[1].Split(',') |> Array.map(fun item -> int64(string(item)))
    let mutable inspectCount = 0
    let mutable items = List.ofArray(startingItems)
    member this.incrementInspectCount = fun() -> inspectCount <- inspectCount + 1
    member this.getInspectionCount = fun() -> inspectCount
    member this.getItems = fun() ->
        let returnItems = items
        items <- []
        returnItems
    member this.addItem = fun(item : int64) -> items <- items @ [item]
    member this.operation = fun (old : int64) ->
        let operation = definition.[2].Split(':')
        let equation = operation.[1].Split('=')
        let expression = equation.[1].Split("old").[1].Trim()
        let newValue = match expression.Length with
                        | 1 ->
                            match expression.[0] with
                                | '*' -> old * old
                                | '/' -> old / old
                                | '+' -> old + old
                                | '-' -> old - old
                                | _ -> raise(Exception())
                        | _ ->
                            let value = int64(string(expression.Split(' ').[1]))
                            match expression.[0] with
                                | '*' -> old * value
                                | '/' -> old / value
                                | '+' -> old + value
                                | '-' -> old - value
                                | _ -> raise(Exception())

        newValue
    member this.factor =
        let test = definition.[3].Split(':').[1]
        int64(string(test.Split("divisible by").[1]))
    member this.test = fun (value : int64) ->
        value % this.factor = 0L
    member this.testTrue = int(string(definition.[4].Split(':').[1].Split("throw to monkey").[1]))
    member this.testFalse = int(string(definition.[5].Split(':').[1].Split("throw to monkey").[1]))

let private parseMonkeyDefinitions(input: string[]) : Monkey[] =
      let monkeyDefinitions = input |> Array.splitInto((input.Length + 1) / 7)
      monkeyDefinitions |> Array.map(fun definition -> Monkey(definition))

module Part1 =
    let simulateRounds(monkies: Monkey[], rounds : int, worryDecreaseFactor : float) : unit =
        for _ in 1 .. rounds do
            Array.ForEach(monkies, fun monkey ->
                monkey.getItems() |> List.iter(fun item ->
                    monkey.incrementInspectCount()
                    let newLevel = int64(float(monkey.operation(item)) / worryDecreaseFactor)
                    if newLevel < 0L then
                        (raise(Exception()))
                    let newMonkey = match monkey.test(newLevel) with
                                        | true -> monkey.testTrue
                                        | false -> monkey.testFalse
                    monkies.[newMonkey].addItem(newLevel)
                )
            )

module Part2 =
    let simulateRounds(monkies: Monkey[], rounds : int, worryDecreaseFactor : float) : unit =
        let commonFactor = monkies |> Array.map(fun monkey -> monkey.factor) |> Array.reduce(fun state item -> state * item)
        for _ in 1 .. rounds do
            Array.ForEach(monkies, fun monkey ->
                monkey.getItems() |> List.iter(fun item ->
                    monkey.incrementInspectCount()
                    let newLevel = int64(float(monkey.operation(item)) / worryDecreaseFactor)
                    if newLevel < 0L then
                        (raise(Exception()))
                    let newMonkey = match monkey.test(newLevel) with
                                        | true -> monkey.testTrue
                                        | false -> monkey.testFalse
                    monkies.[newMonkey].addItem(newLevel % commonFactor)
                )
            )

let calculateMonkeyBusiness(monkies: Monkey[], rounds: int, worryDecreaseFactory : float, simulateRounds:(Monkey[] * int * float) -> unit) : int64 =
    simulateRounds(monkies, rounds, worryDecreaseFactory)
    let topMonkies = monkies |> Array.sortByDescending(fun m -> m.getInspectionCount()) |> Array.take 2
    int64(topMonkies.[0].getInspectionCount()) * int64(topMonkies.[1].getInspectionCount())

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")

    let monkies = parseMonkeyDefinitions(input)

    printfn $"Part One level of monkey business: {calculateMonkeyBusiness(monkies, 20, 3.0, Part1.simulateRounds)}"
    printfn $"Part Two level of monkey business: {calculateMonkeyBusiness(monkies, 10000, 1.0, Part2.simulateRounds)}"

    0
