open System
open System.IO

type Point2D = { X: int; Y: int }
type Antenna = { Location: Point2D; Frequency: char }

let file = "example.txt" // part 1: 14, part 2: 
//let file = "input.txt" // part 1: 278, part 2: 
let data = file |> File.ReadAllLines
let map = data |> array2D
let antiNodeMap = Array2D.create (Array2D.length1 map) (Array2D.length2 map) '.'

printfn "%A" map

let antennas = seq {
    for y in 0 .. Array2D.length1 map - 1 do
        for x in 0 .. Array2D.length2 map - 1 do
            if not (map[y, x] = '.') then yield {Location = {X = x; Y = y}; Frequency = map[y, x] }
}
let antennaArray = antennas |> Array.ofSeq

let rec antiNode A B (result: Point2D list) harmonics =
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
        | a, b when a > 0 && b > 0 -> if A.X + dx < Array2D.length1 map && A.Y + dy < Array2D.length2 map then  Some {X = A.X + dx; Y = A.Y + dy} else result
        | _ -> failwith "Invalid case"
    if not harmonics then thisResult
    else if thisResult = None then result
    else antiNode A B (thisResult.Value::result) harmonics

printfn "%A" antennaArray

let antiNodes = seq {
    for i in 0..antennaArray.Length - 1 do
        for j in 0..antennaArray.Length - 1 do
            if not (i = j) && antennaArray[i].Frequency = antennaArray[j].Frequency then yield antiNode antennaArray[i].Location antennaArray[j].Location
}

antiNodes |>  Seq.filter (fun p -> p <> None) |> Seq.distinct |> Seq.length |> printfn "Part 1: %A"

let rec harmonic A B result=
    match antiNode A B with
    | None -> result
    | Some p -> harmonic p A (p::result)

antiNodes |>  Seq.filter (fun p -> p <> None) |> Seq.iter (fun p -> antiNodeMap[p.Value.Y, p.Value.X] <- '#')
printfn "%A" antiNodeMap