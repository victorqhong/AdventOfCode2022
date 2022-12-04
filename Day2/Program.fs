open System

type Choice =
    | Rock = 1
    | Paper = 2
    | Scissors = 3

type Result =
    | Lose = 0
    | Draw = 3
    | Win = 6

let mapChoice(choice: char) : Choice =
    match choice with
        | 'A' | 'X' -> Choice.Rock
        | 'B' | 'Y' -> Choice.Paper
        | 'C' | 'Z' -> Choice.Scissors
        | _ -> raise(Exception())

let mapResult(result: char) : Result =
    match result with
        | 'X' -> Result.Lose
        | 'Y' -> Result.Draw
        | 'Z' -> Result.Win
        | _ -> raise(Exception())

module Part1 =
    let private getShape(play: string) : Choice =
        mapChoice(play.[2])

    let private getResult(play: string) : Result =
        match (mapChoice(play.[0]), mapChoice(play.[2])) with
            | (Choice.Rock, Choice.Rock) -> Result.Draw
            | (Choice.Rock, Choice.Paper) -> Result.Win
            | (Choice.Rock, Choice.Scissors) -> Result.Lose
            | (Choice.Paper, Choice.Rock) -> Result.Lose
            | (Choice.Paper, Choice.Paper) -> Result.Draw
            | (Choice.Paper, Choice.Scissors) -> Result.Win
            | (Choice.Scissors, Choice.Rock) -> Result.Win
            | (Choice.Scissors, Choice.Paper) -> Result.Lose
            | (Choice.Scissors, Choice.Scissors) -> Result.Draw
            | (_, _) -> raise (Exception())

    let calculateScore(input: string[]) : int =
        input |> Array.sumBy(fun x -> int(getShape(x)) + int(getResult(x)))

module Part2 =
    let private calculateShape(opponentChoice: Choice, desiredResult: Result) : Choice =
        match (opponentChoice, desiredResult) with
            | (Choice.Rock, Result.Win) -> Choice.Paper
            | (Choice.Rock, Result.Lose) -> Choice.Scissors
            | (Choice.Rock, Result.Draw) -> Choice.Rock
            | (Choice.Paper, Result.Win) -> Choice.Scissors
            | (Choice.Paper, Result.Lose) -> Choice.Rock
            | (Choice.Paper, Result.Draw) -> Choice.Paper
            | (Choice.Scissors, Result.Win) -> Choice.Rock
            | (Choice.Scissors, Result.Lose) -> Choice.Paper
            | (Choice.Scissors, Result.Draw) -> Choice.Scissors
            | (_, _) -> raise(Exception())

    let private getShape(play: string) : Choice =
        calculateShape(mapChoice(play.[0]), mapResult(play.[2]))

    let private getResult(play: string) : Result =
        mapResult(play.[2])

    let calculateScore(input: string[]) : int =
        input |> Array.sumBy(fun x -> int(getShape(x)) + int(getResult(x)))

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt");

    let score1 = Part1.calculateScore(input)
    printfn $"Calculated part 1 score: {score1}"

    let score2 = Part2.calculateScore(input)
    printfn $"Calculated part 2 score: {score2}"

    0
