let filePath = "./inputs/day2.txt"

let inputs =
    System.IO.File.ReadAllLines(filePath)
    |> Array.map (fun line -> line.Split(' ') |> Array.map int)

let corrctPair a b direction =
    a <> b && ((a > b) = direction) && (abs (a - b)) <= 3

let rec isSafe' a b rest direction =
    match rest with
    | [] -> corrctPair a b direction
    | c :: r -> (corrctPair a b direction) && isSafe' b c r direction

let isSafe lst =
    match lst with
    | [ a; b ] -> (abs (a - b) <= 3)
    | a :: b :: rest -> isSafe' a b rest (a > b)
    | _ -> false

let rec isAlmostSafe' a b rest direction errorCount =
    match (rest, errorCount) with
    | (_, 2) -> false
    | ([], _) ->
        if (corrctPair a b direction) then
            errorCount < 2
        else
            errorCount < 1
    | (c :: r, _) ->
        if (corrctPair a b direction) then
            isAlmostSafe' b c r direction errorCount
        else
            isAlmostSafe' b c r direction (errorCount + 1)

let isAlmostSafe lst =
    match lst with
    | [ a; b ] -> (abs (a - b) <= 3)
    | a :: b :: rest -> isAlmostSafe' a b rest (a > b) 0
    | _ -> false


let part1 =
    Array.fold (fun (acc: int) (el: int[]) -> acc + if (isSafe (Array.toList el)) then 1 else 0) 0 inputs

printfn "Day 2 Part 1: %A" part1

let part2 =
    Array.fold (fun (acc: int) (el: int[]) -> acc + if (isAlmostSafe (Array.toList el)) then 1 else 0) 0 inputs

printfn "Day 2 Part 2: %A" part2
