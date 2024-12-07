open System.IO
open System.Text.RegularExpressions

let filePath = "./inputs/day3.txt"
let fileContent = File.ReadAllText(filePath)

let pattern = @"mul\((\d+),(\d+)\)"
let matches program = Regex.Matches(program, pattern)

let doDont =
    Regex.Replace(fileContent, @"don't\(\)((?:[^d]|d(?!o\(\)))*)(do\(\)?)", "")

let multiplicationStrings program : string array =
    [| for m in matches program do
           yield m.Value |]


let multi (s: string) =
    let parts =
        s.Split([| '('; ','; ')' |], System.StringSplitOptions.RemoveEmptyEntries)

    if parts.Length = 3 && parts.[0] = "mul" then
        let a = int parts.[1]
        let b = int parts.[2]
        a * b
    else
        0

let part1 = multiplicationStrings fileContent |> Array.sumBy multi


printfn "Day 3, Part 1: %d" part1

let part2 = multiplicationStrings doDont |> Array.sumBy multi


printfn "Day 3, Part 2: %A" part2
