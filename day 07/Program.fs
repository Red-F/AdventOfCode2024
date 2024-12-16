open System
open System.IO

// let file = "example.txt" // part 1: 3749L, part 2: 11387L
let file = "input.txt" // part 1: 303766880536L, part 2: 337041851384440L
let data = file |> File.ReadAllLines |> Seq.map (fun s -> s.Split(":", StringSplitOptions.RemoveEmptyEntries))  |> Seq.map (fun a -> (int64 a[0], a[1].Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.toList |> List.map int64 )) |> Seq.toArray

let solvePart1 (d: int64 * int64 list) =
    let rec solve' (expected: int64) (a: int64 list) (depth: int64) (result: int64) =
        match result with
        | p when a = [] && p = expected -> true
        | _ when a = [] -> false
        | _ -> solve' expected a.Tail depth (result + a.Head) || solve' expected a.Tail depth (result * a.Head) 
    solve' (d |> fst) (d |> snd |> List.tail) 0L (d |> snd |> List.head)
    
data |> Array.filter (fun d -> d |> solvePart1) |> Array.sumBy fst |> printfn "Part 1: %A"

let solvePart2 (d: int64 * int64 list) =
    let rec solve' (expected: int64) (a: int64 list) (depth: int64) (result: int64) =
        match result with
        | p when a = [] && p = expected -> true
        | _ when a = [] -> false
        | _ -> solve' expected a.Tail depth (result + a.Head) || solve' expected a.Tail depth (result * a.Head) || solve' expected a.Tail depth (int64 ((string result) + (string a.Head))) 
    solve' (d |> fst) (d |> snd |> List.tail) 0L (d |> snd |> List.head)
    
data |> Array.filter (fun d -> d |> solvePart2) |> Array.sumBy fst |> printfn "Part 2: %A"
