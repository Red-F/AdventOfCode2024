open System
open System.IO

type Point2D = { X: int; Y: int }
type Cell = { C : char; Visited : bool }
// let file = "example.txt" // part 1: 1930L, part 2: ?
let file = "input.txt" // part 1: 1396298L, part 2: ?
let data = file |> File.ReadAllLines |> Array.map (fun s -> Seq.map (fun c -> {C = c; Visited = false}) s |> Seq.toArray) |> array2D

let stepsA c (p: Point2D)= 
    seq { yield { p with X = p.X + 1 }
          yield { p with X = p.X - 1 }
          yield { p with Y = p.Y + 1 }
          yield { p with Y = p.Y - 1 } } |> Seq.filter (fun p -> p.X >= 0 && p.Y >= 0 && p.X < Array2D.length2 data && p.Y < Array2D.length1 data && data[p.Y, p.X].C = c && not data[p.Y, p.X].Visited)
let AreaA (p: Point2D) =
    let rec Area' (p: Point2D) (c: char) (r: Point2D list) =
        let nextSteps = stepsA c p
        match nextSteps |> Seq.isEmpty with
        | true -> match data[p.Y, p.Y].Visited with | false -> data[p.Y, p.X] <- {data[p.Y, p.X] with Visited = true}; r | true -> r
        | false -> data[p.Y, p.X] <- {data[p.Y, p.X] with Visited = true}; nextSteps |> Seq.fold (fun acc f -> acc @ Area' f c r) [] |> (fun f -> f @[p]) 
    data[p.Y, p.X] <- {data[p.Y, p.X] with Visited = true}; Area' p data[p.Y, p.X].C [p]

let steps (p: Point2D)= 
    seq { yield { p with X = p.X + 1 }
          yield { p with X = p.X - 1 }
          yield { p with Y = p.Y + 1 }
          yield { p with Y = p.Y - 1 } }

let rec Area (p: Point2D) =
    let rec Area' c (pi: Point2D) (r: Point2D list) =
        match pi with
        | _ when pi.X < 0 || pi.Y < 0 || pi.X >= Array2D.length2 data || pi.Y >= Array2D.length1 data || data[pi.X, pi.Y].C <> c || data[pi.X, pi.Y].Visited -> []
        | _ -> data[pi.X, pi.Y] <- {data[pi.X, pi.Y] with Visited = true}; steps pi |> Seq.fold (fun acc f -> acc @ Area' c f r) [] |> (fun l -> l @ [pi])
    Area' data[p.X, p.Y].C p []

let perimeter c (l: Point2D list) = l |> List.fold (fun acc p -> acc + (steps p |> Seq.fold (fun acc2 elem -> acc2 + match List.exists (fun n -> n = elem) l with | true -> 0L | false -> 1L) 0L ) ) 0L

// data |> printfn "%A"
let areaTypes = seq { for y in 0 .. Array2D.length1 data - 1 do for x in 0 .. Array2D.length2 data - 1 do if not data[x,y].Visited then yield data[x,y].C, Area {X = x; Y = y}}
// steps { X = 0; Y = 0 } |> Seq.iter (fun p -> printfn "%A" p)
//Area { X = 0; Y = 0 } |> List.sortBy _.Y  |> List.length |> printfn "Part 1: %A"
// areaTypes |> Seq.iter (fun t -> printfn "%c %A" (fst t) (snd t |> List.length))
//areaTypes |> Seq.iter (fun t -> (fst t, (snd t |> List.length |> int64), perimeter (fst t) (snd t)) |> printfn "Part 1: %A")
areaTypes |> Seq.map (fun t -> (snd t |> List.length |> int64) * perimeter (fst t) (snd t)) |> Seq.sum |> printfn "Part 1: %A"