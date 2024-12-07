open System
open System.IO

//let file = "example.txt" // part 1: 11, part 2: 31
let file = "input.txt" // part 1: 1603498, part 2: 25574739
let data = file |> File.ReadAllLines |> Seq.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries)) |> Seq.map (fun a -> (int a[0], int a[1])) |> Seq.toList
let leftData = data |> List.map fst |> List.sort
let rightData = data |> List.map snd |> List.sort
List.fold2 (fun acc l r -> acc + Math.Abs (int l - r)) 0 leftData rightData |> printfn "Part 1: %A"

let counts = rightData |> List.countBy id
List.fold (fun acc l -> acc + (counts |> List.tryFind(fun (x, _) -> x = l) |> (fun p -> match p with | None -> 0 | Some v -> v |> snd |> (*) l))) 0 leftData |> printfn "Part 2: %A"