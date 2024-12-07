open System.IO

let filePath = "./inputs/day4.txt"

let readFileToArray (path: string) =
    File.ReadAllLines(path) |> Array.map (fun line -> line.ToCharArray())

let letters = readFileToArray filePath

let countInRow (row: char array) (i: int) =
    let checkUp i j =
        if
            i >= 3
            && letters.[i - 1].[j] = 'M'
            && letters.[i - 2].[j] = 'A'
            && letters.[i - 3].[j] = 'S'
        then
            1
        else
            0

    let checkUpRight i j =
        if
            i >= 3
            && row.Length - j >= 3
            && letters.[i - 1].[j + 1] = 'M'
            && letters.[i - 2].[j + 2] = 'A'
            && letters.[i - 3].[j + 3] = 'S'
        then
            1
        else
            0


    let checkRight i j =
        if
            row.Length - j >= 3
            && letters.[i].[j + 1] = 'M'
            && letters.[i].[j + 2] = 'A'
            && letters.[i].[j + 3] = 'S'
        then
            1
        else
            0


    let checkDownRight i j =

        if
            row.Length - j > 3
            && letters.Length - i > 3
            && letters.[i + 1].[j + 1] = 'M'
            && letters.[i + 2].[j + 2] = 'A'
            && letters.[i + 3].[j + 3] = 'S'
        then
            1
        else
            0

    let checkDown i j =
        if
            letters.Length - i > 3
            && letters.[i + 1].[j] = 'M'
            && letters.[i + 2].[j] = 'A'
            && letters.[i + 3].[j] = 'S'
        then
            1
        else
            0

    let checkDownLeft i j =
        if
            j >= 3
            && letters.Length - i > 3
            && letters.[i + 1].[j - 1] = 'M'
            && letters.[i + 2].[j - 2] = 'A'
            && letters.[i + 3].[j - 3] = 'S'
        then
            1
        else
            0

    let checkLeft i j =
        if
            j >= 3
            && letters.[i].[j - 1] = 'M'
            && letters.[i].[j - 2] = 'A'
            && letters.[i].[j - 3] = 'S'
        then
            1
        else
            0

    let checkUpLeft i j =
        if
            j >= 3
            && i >= 3
            && letters.[i - 1].[j - 1] = 'M'
            && letters.[i - 2].[j - 2] = 'A'
            && letters.[i - 3].[j - 3] = 'S'
        then
            1
        else
            0

    let isStartXmas i j =
        checkUp i j
        + checkUpRight i j
        + checkRight i j
        + checkDownRight i j
        + checkDown i j
        + checkDownLeft i j
        + checkLeft i j
        + checkUpLeft i j

    Array.mapi (fun j el -> if el <> 'X' then 0 else isStartXmas i j) row
    |> Array.sum

let countXMasRow (row: char array) (i: int) =
    let isXMas i j =
        if
            (letters.[i - 1].[j - 1] = 'M' && letters.[i + 1].[j + 1] = 'S'
             || letters.[i - 1].[j - 1] = 'S' && letters.[i + 1].[j + 1] = 'M')
            && (letters.[i + 1].[j - 1] = 'M' && letters.[i - 1].[j + 1] = 'S'
                || letters.[i + 1].[j - 1] = 'S' && letters.[i - 1].[j + 1] = 'M')
        then
            1
        else
            0

    Array.mapi
        (fun j el ->
            if el = 'A' && j > 0 && j < row.Length - 1 then
                isXMas i j
            else
                0)
        row
    |> Array.sum

let countXMas row i =
    if i > 0 && i < letters.Length - 1 then
        countXMasRow row i
    else
        0

let part1 = letters |> Array.mapi (fun i row -> countInRow row i) |> Array.sum

printfn "Day 4, Part 1: %A" part1

let part2 = letters |> Array.mapi (fun i row -> countXMas row i) |> Array.sum

printfn "Day 4, Part 2: %A" part2
