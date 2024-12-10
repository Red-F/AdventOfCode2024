open System.Text.RegularExpressions

// let file = "example.txt" // part 1: 161
// let file = "example2.txt" // part 1: 161, part 2: 48
let file = "input.txt" // part 1: 187825547, part 2: 85508223
let data = file |> System.IO.File.ReadAllText
let matches pattern input = seq { for m in  Regex.Matches(input, pattern) do yield m }

data |> matches "mul\(([0-9]{1,3}),([0-9]{1,3})\)" |> Seq.map (fun m -> int m.Groups[1].Value * int m.Groups[2].Value) |> Seq.sum |> printfn "part 1: %A"

data |> matches "mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\\)|don't\(\)" |> Seq.fold (fun (enabled, sum) elem -> match elem.Groups[0].Value with | "do()" -> (true, sum) | "don't()" -> (false, sum) | _ -> (enabled, sum + match enabled with | false -> 0 | true -> int elem.Groups[1].Value * int elem.Groups[2].Value)) (true, 0) |> snd |>  printfn "part 2: %A"
 