open System
open System.IO

// let file = "example.txt" // part 1: 55312, part 2: ?
let file = "input.txt" // part 1: 220722, part 2: 261952051690787
let data = file |> File.ReadAllText |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList

let blinkStone s = match s with | "0" -> ["1"] | _ when s.Length % 2 = 0 -> [int64 s[..(s.Length/2 - 1)] |> string; int64 s[(s.Length/2)..] |> string] | _ -> [(int64 s) * 2024L |> string]
let rec blinkBrute count l = match count with | 0 -> l | _ -> l |> List.fold (fun acc s -> acc @ blinkStone s) [] |> blinkBrute (count - 1)

// let's brute force part 1, this will probably not work for part 2
data |> List.map (fun s -> blinkBrute 25 [s] |> List.length) |> List.sum |> printfn "Part 1: %A"

// as we saw order is not important, so we can use a map to store the stone counts
let stoneMap = data |> List.map (fun s -> (s, 1L)) |> Map.ofList
let blink (map: Map<string, int64>) =
    let tryAddValue k v (map: Map<string, int64>) = match map.TryGetValue(k) with | true, x -> Map.add k (v+x) map | _ -> Map.add k v map
    map |>
    Map.fold
        (fun (acc: Map<string,int64>) k v ->
        match k with
        | "0" -> acc |> tryAddValue "1" v
        | _ when k.Length % 2 = 0 -> acc |> tryAddValue (int64 k[..(k.Length/2 - 1)] |> string) v |> tryAddValue (int64 k[(k.Length/2)..] |> string) v
        | _ -> acc |> tryAddValue ((int64 k) * 2024L |> string) v) Map.empty<string, int64>

let rec blinks count (map: Map<string, int64>)  =
    match count with
    | 0 -> map
    | _ -> map |> blink |> blinks (count - 1)

stoneMap |> blinks 75 |> Map.values |> Seq.sum  |> printfn "Part 2: %A"