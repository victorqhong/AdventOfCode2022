open System

type Directory = { Path: string; Parent: Option<Directory>; mutable Directories: Directory list; mutable Files: File list }
and File = { Name: string; Size: int }

let private root = { Path = ""; Parent = None; Directories = []; Files = [] }

let private createDirectory(currentDirectory: Directory, input: string[]) : Directory =
    let (directories, files) = input |> Array.partition(fun x -> x.StartsWith("dir"))

    currentDirectory.Directories <- directories |> Array.map(fun x ->
        let elements = x.Split(' ')
        let path = 
            match currentDirectory.Path with
                | "" -> $"/{currentDirectory.Path}{elements.[1]}"
                | _ -> $"{currentDirectory.Path}/{elements.[1]}"
        { Path = path; Parent = Some(currentDirectory); Directories = []; Files = [] }) |> List.ofArray;

    currentDirectory.Files <- files |> Array.map(fun x -> 
        let elements = x.Split(' ')
        { Name = elements.[1]; Size = int(elements.[0]) }) |> List.ofArray 

    currentDirectory

let rec private makeFileSystem(currentDirectory: Directory, input: string[]) : Directory =
    if input.Length = 0 then root else

    let currentLine = input |> Array.head
    let restOfLines = input |> Array.tail
    match currentLine.[0..3] with
        | "$ cd" -> 
            match currentLine.[5..] with
                | "/" -> makeFileSystem(root, restOfLines)
                | ".." -> makeFileSystem(currentDirectory.Parent.Value, restOfLines)
                | _ ->
                    let directoryName = currentLine.[5..]
                    let directory = currentDirectory.Directories |> List.find(fun x -> x.Path.EndsWith(directoryName))
                    makeFileSystem(directory, restOfLines)
        | "$ ls" -> 
            let outputEndIndex = restOfLines |> Array.tryFindIndex(fun x -> x.StartsWith("$"))
            match outputEndIndex with
                | None -> makeFileSystem(createDirectory(currentDirectory, restOfLines), [||])
                | Some(index) -> makeFileSystem(createDirectory(currentDirectory, restOfLines.[0..(index - 1)]), restOfLines.[index..])
        | _ -> raise(Exception())

let rec private calculateDirectorySize(directory: Directory) : int =
    let fileSizes = directory.Files |> List.sumBy(fun x -> x.Size)
    let directorySizes = directory.Directories |> List.sumBy(fun x -> calculateDirectorySize(x))
    fileSizes + directorySizes

let rec private getDirectories(directory: Directory) : Directory list =
    [directory] @ (directory.Directories |> List.collect(fun x -> getDirectories(x)))

module Part1 =
    let private upperLimit = 100000

    let sumOfDirectorySizes(fileSystem: Directory) : int =
        let directories = getDirectories(fileSystem)
        directories |> List.sumBy(fun x -> 
            let size = calculateDirectorySize(x)
            if size > upperLimit then 0 else size
        )

module Part2 =
    let private totalDiskSpace = 70000000
    let private updateSize = 30000000

    let sizeChosenDirectory(fileSystem: Directory) : int =
        let usedDiskSpace = calculateDirectorySize(fileSystem)
        let unusedDiskSpace = totalDiskSpace - usedDiskSpace
        let neededSpace = updateSize - unusedDiskSpace

        let directories = getDirectories(fileSystem)
        let sizes = directories |> List.map(fun x -> calculateDirectorySize(x)) |> List.filter(fun x -> x >= neededSpace) |> List.sort

        sizes |> List.head

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllLines("input.txt")    

    let fileSystem = makeFileSystem(root, input)

    printfn $"Sum of directory total sizes: {Part1.sumOfDirectorySizes(fileSystem)}"
    printfn $"Size of directory: {Part2.sizeChosenDirectory(fileSystem)}"

    0
