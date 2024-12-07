let filePath = "./inputs/day1.txt"

let readFileAndParseToTuples (path: string) =
    System.IO.File.ReadAllLines(path)
    |> Array.fold
        (fun acc line ->
            let parts =
                line.Split([| ' '; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int

            (parts.[0] :: fst acc, parts.[1] :: snd acc))
        ([], [])
    |> fun (l1, l2) -> (List.toArray l1, List.toArray l2)

let parsedInput = readFileAndParseToTuples filePath

let fstArr: int array = parsedInput |> fst |> Array.sort
let sndArr: int array = parsedInput |> snd |> Array.sort

let part1 =
    Array.fold (fun acc (a, b) -> acc + abs (a - b)) 0 (Array.zip fstArr sndArr)

printfn "Day 1, Part 1: %d" part1

let countInSndArr = Array.countBy (fun el -> el) sndArr |> Map.ofArray

let part2 =
    fstArr |> Array.sumBy (fun i -> i * defaultArg (Map.tryFind i countInSndArr) 0)

printfn "Day 1, Part 2: %d" part2
