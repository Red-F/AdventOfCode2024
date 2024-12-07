open System
open System.IO

//let file = "example.txt" // part 1: 11, part 2: 31
let file = "input.txt" // part 1: 1603498, part 2: 25574739
// Test performance with big file (unzip it first)
//let file = "bigboy.txt" // part 1: 70030075280L, part 2: 112445724586901L
let leftData, rightData = file |> File.ReadAllLines |> Seq.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries)) |> Seq.map (fun a -> (int64 a[0], int64 a[1])) |> Seq.toArray |> Array.unzip
Array.fold2 (fun acc l r -> acc + Math.Abs (int64 l - r)) 0L (leftData |> Array.sort) (rightData |> Array.sort) |> printfn "Part 1: %A"

let maxLocation = [rightData |> Array.max; leftData |> Array.max] |> List.max
let countsArray = Array.create (maxLocation |> int) 0
let counts = rightData |> Array.countBy id |> Array.iter (fun (x, y) -> countsArray[int x-1] <- y)
Array.fold (fun acc l -> acc + (countsArray[int l-1] |> int64 |> (*) l)) 0L leftData |> printfn "Part 2: %A"