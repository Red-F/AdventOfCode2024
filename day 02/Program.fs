open System
open System.IO

// let file = "example.txt" // part 1: 2, part 2: 4
let file = "input.txt" // part 1: ?, part 2: 540
let data = file |> File.ReadAllLines |> Seq.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int64)

let isSafe (x: int64 array) = (x[0] <> x[1]) && (x |> fun (a:int64 array) -> ((a[1] - a[0])/Math.Abs(a[1] - a[0]), Array.pairwise a) |> fun (upOrDown, a) -> Array.fold (fun acc (l, r) -> acc && match (upOrDown * (r - l)) with | 1L | 2L | 3L -> true | _ -> false) true a) 
data |> Seq.map isSafe |> Seq.filter id |> Seq.length |> printfn "part 1: %A"

data |> Seq.map (fun a -> seq { yield a; for i in 0 .. a.Length - 1 do yield a |> Array.removeAt i }) |> Seq.map (fun a -> Seq.map isSafe a |> Seq.fold (fun acc x -> acc || x) false)|> Seq.filter id |> Seq.length |> printfn "part 2: %A"