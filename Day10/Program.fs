open System

let registerValueAtCycle(instructions: string[], cycle: int) : int =
    let mutable register = 1
    let mutable stackPointer = 0
    let mutable instruction = [||]
    let mutable timer = -1

    for i in 1 .. cycle do
        if timer > 0 then
            timer <- timer - 1
        else
            if timer = 0 then
                register <- register + int(string(instruction.[1]))
                stackPointer <- stackPointer + 1
                timer <- -1
            
            instruction <- instructions.[stackPointer].Split(' ')
            match instruction.[0] with
                | "noop" ->
                    stackPointer <- stackPointer + 1
                | "addx" ->
                    timer <- 1
                | _ -> raise(Exception())

    register

module Part1 = 
    let signalStrength(registerValue: int, cycle: int) : int =
        registerValue * cycle

    let sumOfSignalStrengths(input: string[], sampleCycles: int[]) : int =
        sampleCycles |> Array.map(fun cycle -> signalStrength(registerValueAtCycle(input, cycle), cycle)) |> Array.sum

module Part2 =
    let drawBuffer(input: string[], bufferWidth: int, bufferHeight: int) : unit =
        for j in 0 .. (bufferHeight - 1) do
            for i in 0 .. (bufferWidth - 1) do
                let index = j * bufferWidth + i
                let cycle = index + 1
                let registerValue = registerValueAtCycle(input, cycle)
                if  (registerValue - 1 <= i) && (i <= registerValue + 1) then
                    printf "#"
                else
                    printf "."
            printfn ""

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    
    printfn $"Sum of signal strengths: {Part1.sumOfSignalStrengths(input, [| 20; 60; 100; 140; 180; 220 |])}"
    Part2.drawBuffer(input, 40, 6)

    0
