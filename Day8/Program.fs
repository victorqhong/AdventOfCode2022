open System

let private makeGrid(input: string[]) : int[,] =
    let n = input.[0].Length
    Array2D.init n n (fun i j -> int(input.[j].[i]) - int('0'))

module Part1 =
    let private isVisible(x: int, y: int, height: int, grid: int[,]) : int =
        if x = 0 then 1 else
        if y = 0 then 1 else
        if x = grid.GetUpperBound(0) then 1 else
        if y = grid.GetUpperBound(1) then 1 else
        
        let left = grid.[0..(x-1), y]
        let right = grid.[(x+1)..(grid.GetUpperBound(0)), y]
        let top = grid.[x, 0..(y-1)]
        let bottom = grid.[x, (y+1)..(grid.GetUpperBound(1))]

        let isHeightVisible(x: int) = x < height
        let leftVisible = left |> Array.forall isHeightVisible
        let rightVisible = right |> Array.forall isHeightVisible
        let topVisible = top |> Array.forall isHeightVisible
        let bottomVisible = bottom |> Array.forall isHeightVisible

        if leftVisible || rightVisible || topVisible || bottomVisible then 1 else 0

    let visibleTrees(grid: int[,]) : int =
        let visibility = grid |> Array2D.mapi(fun i j h -> isVisible(i, j, h, grid))
        let indices = [| 0..(visibility.GetUpperBound(0)) |] 
        let sums = indices |> Array.map(fun x -> visibility.[x, *] |> Array.sum)
        let sum = sums |> Array.sum
        sum

module Part2 =
    let calculateScore(values: int[], height: int) : int =
        let index = values |> Array.tryFindIndex(fun x -> x >= height)
        1 + match index with
            | None -> values.Length - 1
            | Some(i) -> i

    let treeScore(x: int, y: int, height: int, grid: int[,]) : int =
        if x = 0 then 0 else
        if y = 0 then 0 else
        if x = grid.GetUpperBound(0) then 0 else
        if y = grid.GetUpperBound(1) then 0 else

        let left = grid.[0..(x-1), y] |> Array.rev
        let right = grid.[(x+1)..(grid.GetUpperBound(0)), y]
        let top = grid.[x, 0..(y-1)] |> Array.rev
        let bottom = grid.[x, (y+1)..(grid.GetUpperBound(1))]

        let leftScore = calculateScore(left, height)
        let rightScore = calculateScore(right, height)
        let topScore = calculateScore(top, height)
        let bottomScore = calculateScore(bottom, height)
        
        leftScore * rightScore * topScore * bottomScore

    let highestScore(grid: int[,]) : int = 
        let scores = grid |> Array2D.mapi(fun i j h -> treeScore(i, j, h, grid))
        let indices = [| 0..(grid.GetUpperBound(0)) |]
        let maxes = indices |> Array.map(fun x -> scores.[x, *] |> Array.max)
        let max = maxes |> Array.max
        max

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt");
    let grid = makeGrid input

    printfn $"Visible trees: {Part1.visibleTrees(grid)}"
    printfn $"Highest score: {Part2.highestScore(grid)}"

    0
