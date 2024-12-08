open System
open System.IO

type Point2D = { X: int; Y: int }
type Antenna = { Location: Point2D; Frequency: char }

let file = "example.txt" // part 1: 11, part 2: 31
let data = file |> File.ReadAllLines
let map = data |> array2D
let antiNodeMap = Array2D.create (Array2D.length1 map) (Array2D.length2 map) '.'

let antennas = seq {
    for y in 0 .. Array2D.length1 map - 1 do
        for x in 0 .. Array2D.length2 map - 1 do
            if not (map[y, x] = '.') then yield {Location = {X = x; Y = y}; Frequency = map[y, x] }
}
let antennaArray = antennas |> Array.ofSeq

let antiNodes A B =
    let dx = Math.Abs (A.X - B.X)
    let dy = Math.Abs (A.Y - B.Y)
    match A.X - B.X, A.Y - B.Y with
    | 0, a when a < 0 -> {X = A.X; Y = (min A.Y B.Y) - dy}
    | 0, _ -> {X = A.X; Y = (max A.Y B.Y) + dy}
    | a, 0 when a < 0 -> {X = (min A.X B.X) - dx; Y = A.Y}
    | _, 0 -> {X = (max A.X B.X) + dx; Y = A.Y}
    | a, b when a < 0 && b < 0 -> {X = A.X - dx; Y = A.Y - dy}
    | a, b when a > 0 && b < 0 -> {X = A.X + dx; Y = A.Y - dy}
    | a, b when a < 0 && b > 0 -> {X = A.X - dx; Y = A.Y + dy}
    | a, b when a > 0 && b > 0 -> {X = A.X + dx; Y = A.Y + dy}
    | _ -> failwith "Invalid case"

//printfn "%A" antennaArray

let z = seq {
    for i in 0..antennaArray.Length - 1 do
        for j in 0..antennaArray.Length - 1 do
            if not (i = j) && antennaArray[i].Frequency = antennaArray[j].Frequency then yield antiNodes antennaArray[i].Location antennaArray[j].Location
}

z |>  Seq.filter (fun p -> p.X >= 0 && p.X < Array2D.length1 map && p.Y >= 0 && p.Y < Array2D.length2 map) |> Seq.toList |> List.distinct |> List.length |> printfn "Part 1: %A"

z |>  Seq.filter (fun p -> p.X >= 0 && p.X < Array2D.length1 map && p.Y >= 0 && p.Y < Array2D.length2 map) |> Seq.iter (fun p -> antiNodeMap.[p.Y, p.X] <- '#')
//printfn "%A" antiNodeMap