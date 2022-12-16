open System

type Knot = { x: int; y: int }

let parseInput(input: string[]) : (char * int)[] =
    input |> Array.map(fun x -> (x.[0], int(string(x.[2..]))))

let isAdjacent(head: Knot, tail: Knot) : (bool * Knot) =
    let difference = { x = head.x - tail.x; y = head.y - tail.y }
    if (difference.x = 1 || difference.x = 0 || difference.x = -1) && (difference.y = 1 || difference.y = 0 || difference.y = -1) then 
        (true, difference)
    else
        (false, difference)

let calculateTailPosition(newHead: Knot, currentTail: Knot) : Knot =
    match isAdjacent(newHead, currentTail) with
        | (true, _) -> currentTail
        | (false, difference) -> { x = currentTail.x + Math.Clamp(difference.x, -1, 1); y = currentTail.y + Math.Clamp(difference.y, -1, 1) }

let calculateHeadPositions(head: Knot, move: char * int) : Knot[] =
    let (direction, distance) = move
    [| 1 .. distance |] |> Array.map(fun i ->
        match direction with
            | 'L' -> { x = head.x - i; y = head.y }
            | 'R' -> { x = head.x + i; y = head.y }
            | 'U' -> { x = head.x; y = head.y + i }
            | 'D' -> { x = head.x; y = head.y - i }
            | _ -> raise(Exception())
    )

let countUniqueTailPositions(moves: (char * int)[], knotCount : int) : int =
    let mutable knots = [| 1..knotCount |] |> Array.map(fun i -> { x = 0; y = 0 })
    let mutable tailPositions = [knots.[knots.Length - 1]] : Knot list
    Array.ForEach(moves, fun move ->
        let headPositions = calculateHeadPositions(knots.[0], move)
        Array.ForEach(headPositions, fun p ->
            knots.[0] <- p
            for i in 1..(knotCount - 1) do
                knots.[i] <- calculateTailPosition(knots.[i - 1], knots.[i])

            let tail = knots.[knots.Length - 1]
            if (tailPositions |> List.contains(tail) = false) then
                tailPositions <- tailPositions @ [tail]
        )
    )

    tailPositions |> List.length

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")
    let moves = parseInput(input)

    printfn $"Unique tail positions (2 knots): {countUniqueTailPositions(moves, 2)}"
    printfn $"Unique tail positions (10 knots): {countUniqueTailPositions(moves, 10)}"

    0
