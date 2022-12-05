open System
open System.Collections.Generic

let parseStacks(input: string[]) : Stack<char>[] =
    let indices = input.[input.Length - 1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map(fun s -> int(s) - 1)

    let mapLine(line: string) : char[] = line.ToCharArray() |> Array.splitInto(indices.Length) |> Array.map(Array.item(1))
    let parsedLines = input |> Array.take(input.Length - 1) |> Array.map mapLine
    let stacks = indices |> Array.map(fun index -> parsedLines |> Array.map(fun chars -> chars.[index]))

    stacks |> Array.map(fun stack -> stack |> Array.rev |> Array.filter(fun char -> char <> ' ') |> Stack)

let parseProcedures(input: string[]) : (int * int * int) [] =
    input |> Array.take (input.Length - 1) |> Array.map(fun x -> x.Split(' ')) |> Array.map(fun x -> (int(x.[1]), int(x.[3]) - 1, int(x.[5]) - 1))

let topOfEachStack(stacks: Stack<char>[]) : string =
    new string(stacks |> Array.map(fun stack -> stack.Peek()))

let applyProcedures(stacks: Stack<char>[], procedures: (int * int * int)[], applyProcedure: (Stack<char>[] * (int * int * int) -> unit)) : unit =
    Array.ForEach(procedures, fun procedure -> applyProcedure(stacks, procedure))

let getTopCrates(input: string, applyProcedure: (Stack<char>[] * (int * int * int)) -> unit) : string =
    let elements = input.Split("\n\n")
    let stacks = parseStacks (elements.[0].Split('\n'))
    let procedures = parseProcedures (elements.[1].Split('\n'))
    applyProcedures(stacks, procedures, applyProcedure)
    topOfEachStack(stacks)

module Part1 =
    let applyProcedure(stacks: Stack<char>[], procedure: int * int * int) : unit =
        let (move, source, destination) = procedure
        for _ in 1 .. move do
            stacks.[destination].Push(stacks.[source].Pop())

module Part2 =
  let applyProcedure(stacks: Stack<char>[], procedure: int * int * int) : unit =
      let (move, source, destination) = procedure
      let values = ' ' |> Array.create move
      for i in 0 .. (move - 1) do
        values.[i] <- stacks.[source].Pop()
      for i in (move - 1) .. -1 .. 0 do
        stacks.[destination].Push(values.[i])

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllText("input.txt");

    let topCrates1 = getTopCrates(input, Part1.applyProcedure)
    printfn $"Crates on top of each stack part 1: {topCrates1}"

    let topCrates2 = getTopCrates(input, Part2.applyProcedure)
    printfn $"Crates on top of each stack part 2: {topCrates2}"

    0
