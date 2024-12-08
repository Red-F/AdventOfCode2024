open System
open System.IO

type Point2D = { X: int; Y: int }
type Antenna = { Location: Point2D; Frequency: char }

// let file = "example.txt" // part 1: 14, part 2: 34
let file = "input.txt" // part 1: 278, part 2: 1067
let data = file |> File.ReadAllLines
let map = data |> array2D
let antiNodeMap = Array2D.create (Array2D.length1 map) (Array2D.length2 map) '.'

let antennas = seq {
    for y in 0 .. Array2D.length1 map - 1 do
        for x in 0 .. Array2D.length2 map - 1 do
            if not (map[y, x] = '.') then yield {Location = {X = x; Y = y}; Frequency = map[y, x] }
}
let antennaArray = antennas |> Array.ofSeq

let rec antiNode A B result harmonics =
    let dx = Math.Abs (A.X - B.X)
    let dy = Math.Abs (A.Y - B.Y)
    let thisResult =
        match A.X - B.X, A.Y - B.Y with
        | 0, a when a < 0 -> if (min A.Y B.Y) - dy >= 0 then Some {X = A.X; Y = (min A.Y B.Y) - dy} else None
        | 0, _ -> if (max A.Y B.Y) + dy < Array2D.length2 map then Some {X = A.X; Y = (max A.Y B.Y) + dy} else None
        | a, 0 when a < 0 -> if (min A.X B.X) - dx >= 0 then Some {X = (min A.X B.X) - dx; Y = A.Y} else None
        | _, 0 -> if (max A.X B.X) + dx < Array2D.length1 map then  Some {X = (max A.X B.X) + dx; Y = A.Y} else None
        | a, b when a < 0 && b < 0 -> if A.X - dx >= 0 && A.Y - dy >= 0 then  Some {X = A.X - dx; Y = A.Y - dy} else None
        | a, b when a > 0 && b < 0 -> if A.X + dx < Array2D.length1 map && A.Y - dy >= 0 then  Some {X = A.X + dx; Y = A.Y - dy} else None
        | a, b when a < 0 && b > 0 -> if A.X - dx >= 0 && A.Y + dy < Array2D.length2 map then  Some {X = A.X - dx; Y = A.Y + dy} else None
        | a, b when a > 0 && b > 0 -> if A.X + dx < Array2D.length1 map && A.Y + dy < Array2D.length2 map then  Some {X = A.X + dx; Y = A.Y + dy} else None
        | _ -> failwith "Invalid case"
    if thisResult = None then result
    else if not harmonics then match thisResult with | None -> result | Some p -> [p]
    else antiNode thisResult.Value A (thisResult.Value::result) harmonics

let antiNodes harmonics = seq {
    for i in 0..antennaArray.Length - 1 do
        for j in 0..antennaArray.Length - 1 do
            if not (i = j) && antennaArray[i].Frequency = antennaArray[j].Frequency then yield antiNode antennaArray[i].Location antennaArray[j].Location [] harmonics
}

antiNodes false |>  Seq.filter (fun p -> p <> []) |> Seq.distinct |> Seq.length |> printfn "Part 1: %A"

let harmonics = antiNodes true |> Seq.filter (fun p -> p <>[]) |> Seq.fold List.append []
let antiNodeAntennas = antennaArray |> Array.groupBy (fun a -> a.Frequency) |> Array.filter (fun g -> (g |> snd |> Array.length) > 1) |> Array.fold (fun acc g -> List.append acc (g |> snd |> Array.toList)) [] |> List.map _.Location
List.append harmonics antiNodeAntennas |> List.distinct |> List.length |>  printfn "Part 2: %A"