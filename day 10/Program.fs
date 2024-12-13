open System.IO

type Point2D = { X: int; Y: int }

// let file = "example.txt" // part 1: 36, part 2: 81
let file = "input.txt" // part 1: 514, part 2: 1162
let data = file |> File.ReadAllLines
let map = data |> array2D |> Array2D.map (fun c -> match c with | v when v >= '0' && c <= '9' -> int (v - '0') | _ -> -1)

let trailHeads = seq {
    for y in 0 .. Array2D.length1 map - 1 do
        for x in 0 .. Array2D.length2 map - 1 do
            if map[y, x] = 0 then yield {X = x; Y = y}
}

let steps (p: Point2D)= seq {yield {X = p.X + 1; Y = p.Y}; yield {X = p.X - 1; Y = p.Y}; yield {X = p.X; Y = p.Y + 1}; yield {X = p.X; Y = p.Y - 1}} |> Seq.filter (fun p -> p.X >= 0 && p.Y >= 0 && p.X <= map.GetLength(1) - 1 && p.Y <= map.GetLength(0) - 1)
let rec score level (p: Point2D) = match map[p.Y, p.X] with | v when v = level && level =  9 -> [p] | v when v = level -> steps p |> Seq.fold (fun acc x -> acc @ (score (level + 1) x)) [] | _ -> []
let rec rating level (p: Point2D) = match map[p.Y, p.X] with | v when v = level && level =  9 -> 1 | v when v = level -> steps p |> Seq.map (fun x -> rating (level + 1) x) |> Seq.sum | _ -> 0

trailHeads |> Seq.map (fun h -> score 0 h |> List.distinct |> List.length) |> Seq.sum |> printfn "Part 1: %A"

trailHeads |> Seq.map (rating 0) |> Seq.sum |> printfn "Part 2: %A"
