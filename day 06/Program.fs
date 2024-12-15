open System.IO

type Point2D = { X: int; Y: int }
type Cell = | Empty of Visited: bool | Obstacle
type Guard = { X: int; Y: int; Direction: char}

let file = "example.txt" // part 1: 41, part 2: 6
// let file = "input.txt" // part 1: 5318, part 2: 1831
let data = file |> File.ReadAllLines |> array2D
let map = data |> Array2D.map (fun c -> match c with | '#' -> Obstacle | _ -> Empty(false))
let start = seq { for y in 0 .. Array2D.length1 data - 1 do for x in 0 .. Array2D.length2 data - 1 do match data[y, x] with | '.' |'#' -> () | _ -> yield { X = x; Y = y } } |> Seq.head
let guard = { X = start.X; Y = start.Y; Direction = data[start.Y, start.X] }
let turn (guard: Guard) = 
    match guard.Direction with
    | '^' -> { guard with Direction = '>' }
    | '<' -> { guard with Direction = '^' }
    | 'v' -> { guard with Direction = '<' }
    | '>' -> { guard with Direction = 'v' }
    | _ -> failwith "Invalid direction"
let step (guard: Guard) = 
    let x = guard.X
    let y = guard.Y
    match guard.Direction with
    | '^' -> { guard with Y = y - 1 }
    | 'v' -> { guard with Y = y + 1 }
    | '<' -> { guard with X = x - 1 }
    | '>' -> { guard with X = x + 1 }
    | _ -> failwith "Invalid direction"
   
let rec move result (g: Guard) =
    match step g with
    | p when p.X < 0 || p.Y < 0 || p.X >= Array2D.length2 data || p.Y >= Array2D.length1 data -> (result @ [g])
    | p when map[p.Y, p.X] = Obstacle -> g |> turn |> move result
    | p -> move (result @ [g]) p
let route g = move [] g
    
guard |> route |> List.map (fun g -> {X = g.X; Y = g.Y}) |> List.distinct |> List.length |> printfn "Part 1: %A"

let rec detectLoop (result: Guard list) (m: Cell array2d) (g: Guard)=
    match step g with
    | p when p.X < 0 || p.Y < 0 || p.X >= Array2D.length2 data || p.Y >= Array2D.length1 data -> None
    | _ when result |> List.exists (fun n -> n = g) -> result |> Some
    | p when m[p.Y, p.X] = Obstacle -> g |> turn |> detectLoop result m
    | p -> detectLoop (result @ [g]) m p
let tryObstacle (g: Guard) (o: Point2D) =
    map[o.Y, o.X] <- Obstacle
    detectLoop [] map g |> (fun r -> map[o.Y, o.X] <- Empty(false); r)

route guard |> List.map (fun g -> {X = g.X; Y = g.Y}) |> List.distinct |> List.tail |> List.fold (fun acc g -> match tryObstacle guard g with | Some _ -> acc + 1 | None -> acc) 0 |> printfn "Part 2: %A"
