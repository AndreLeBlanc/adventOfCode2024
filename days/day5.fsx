open System.IO

let allLines = File.ReadAllLines("./inputs/day5.txt")

let emptyLineIndex = Array.findIndex System.String.IsNullOrEmpty allLines

let orderingRulesString = allLines.[0 .. emptyLineIndex - 1]
let updatesString = allLines.[emptyLineIndex + 1 ..]

let orderingRules =
    Array.map
        (fun (rule: string) ->
            let parts: string[] = rule.Split([| '|' |])
            (int parts.[0], int parts.[1]))
        orderingRulesString

let mapPages (rules: (int * int)[]) : Map<int, int list> =
    let folder m (k, v) =
        m
        |> Map.change k (function
            | Some old -> Some(v :: old)
            | None -> Some [ v ])

    List.fold folder Map.empty (Array.toList rules)

let updates =
    Array.map (fun (update: string) -> Array.map int (update.Split(","))) updatesString


let rulesMap: Map<int, int list> = mapPages orderingRules

let rec isValid (pages: int[]) (i: int) =
    let pageIsValid (page: int[]) (i: int) =
        match Map.tryFind page.[i] rulesMap with
        | None -> isValid pages (i + 1)
        | Some later ->
            let hasInValid = List.exists (fun x -> Array.contains x page.[0..i]) later
            if hasInValid then false else isValid pages (i + 1)

    match i > pages.Length - 1 with
    | true -> true
    | false -> pageIsValid pages i



let middleSum (row: int[]) =
    match isValid row 0 with
    | true -> row.[row.Length / 2]
    | false -> 0

let part1 = Array.sumBy middleSum updates

printfn "day 5 part 1: %A" part1
