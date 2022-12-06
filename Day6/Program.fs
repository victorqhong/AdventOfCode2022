open System

let findDistinctChars(input: string, length: int) : int =
    let index = [| 0 .. (input.Length - 1) |] |> Array.findIndex(fun x ->
        if x < (length - 1) then false else
            let chars = input.[(x - (length - 1)) .. x]
            let distinct = chars.ToCharArray() |> Array.distinct
            distinct.Length = length
    )

    index + 1

module Part1 =
    let startOfPacket(input: string) : int =
        findDistinctChars(input, 4)
        
module Part2 =
    let startOfMessage(input: string) : int =
        findDistinctChars(input, 14)

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllText("input.txt");

    let startPosition1 = Part1.startOfPacket(input);
    printfn $"Start position of packet: {startPosition1}"

    let startPosition2 = Part2.startOfMessage(input);
    printfn $"Start position of message: {startPosition2}"

    0
