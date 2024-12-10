open System
open System.IO
open System.Text.RegularExpressions

type Point2D = { X: int; Y: int }

// let file = "example.txt" // part 1: 18, part 2:9
let file = "input.txt" // part 1: 2578, part 2: 1972
let data = file |> File.ReadAllLines
let map = data |> array2D
let count pattern input = Regex.Matches(input, pattern) |> _.Count

let leftToRight (m: char array2d) = seq { for y in 0 .. Array2D.length1 m - 1 do yield (m[y, *] |> Array.toSeq |> String.Concat) }
let rightToLeft (m: char array2d) = seq { for y in 0 .. Array2D.length1 m - 1 do yield (m[y, *] |> Array.toSeq |> Seq.rev |> String.Concat) }
let down (m: char array2d) = seq { for x in 0 .. Array2D.length2 m - 1 do yield (m[* , x] |> Array.toSeq |> String.Concat) }
let up (m: char array2d) = seq { for x in 0 .. Array2D.length2 m - 1 do yield (m[* , x] |> Array.toSeq |> Seq.rev |> String.Concat) }

let diagXRightDown x (mat: char [,])= 
    let l = min (mat.GetLength(0) - x) (mat.GetLength(1)) - 1
    seq { for i in 0..l do yield mat[i,x+i] } |> String.Concat
let diagXLeftDown x (mat: char [,])= 
    let l = min x (mat.GetLength(1))
    seq { for i in 0..l do yield mat[i,x-i] } |> String.Concat
let diagYRightDown y (mat: char [,])= 
    let l = min (mat.GetLength(0)) (mat.GetLength(1) - y) - 1
    seq { for i in 0..l do yield mat[y+i,i] } |> String.Concat
let diagYLeftDown y (mat: char [,])= 
    let l = min (mat.GetLength(0)) (mat.GetLength(1)-1-y)
    seq { for i in 0..l do yield mat[y+i,mat.GetLength(1)-1-i] } |> String.Concat
let diagXRightUp x (mat: char [,])= diagXRightDown x mat |> Seq.rev |> String.Concat 
let diagXLeftUp x (mat: char [,])= diagXLeftDown x mat |> Seq.rev |> String.Concat 
let diagYRightUp x (mat: char [,])= diagYRightDown x mat |> Seq.rev |> String.Concat 
let diagYLeftUp x (mat: char [,])= diagYLeftDown x mat |> Seq.rev |> String.Concat 

let diagonals (mat: char [,])= seq{
    for i in 0..mat.GetLength(0) - 1 do yield diagXRightDown i mat
    for i in 1..mat.GetLength(1) - 1 do yield diagYRightDown i mat
    for i in 0..mat.GetLength(0) - 1 do yield diagXLeftDown i mat
    for i in 1..mat.GetLength(1) - 1 do yield diagYLeftDown i mat
    for i in 0..mat.GetLength(0) - 1 do yield diagXRightUp i mat
    for i in 1..mat.GetLength(1) - 1 do yield diagYRightUp i mat
    for i in 0..mat.GetLength(0) - 1 do yield diagXLeftUp i mat
    for i in 1..mat.GetLength(1) - 1 do yield diagYLeftUp i mat
}    

let allSlices(mat: char [,]) = seq {
    yield! diagonals mat
    yield! leftToRight mat
    yield! rightToLeft mat
    yield! down mat
    yield! up mat
}

map |> allSlices |> Seq.map (count "XMAS") |> Seq.sum |> printfn "Part 1: %A"

let findXMAS (mat: char array2d) = seq {
    let combination y x = seq {
        yield String.Concat [mat[y-1, x-1]; mat[y, x]; mat[y+1, x+1]]
        yield String.Concat [mat[y+1, x+1]; mat[y, x]; mat[y-1, x-1]]
        yield String.Concat [mat[y-1, x+1]; mat[y, x]; mat[y+1, x-1]]
        yield String.Concat [mat[y+1, x-1]; mat[y, x]; mat[y-1, x+1]]
    }
    for y in 1..Array2D.length1 mat - 2 do
        for x in 1..Array2D.length2 mat - 2 do
            match combination y x |> Seq.map (count "MAS") |> Seq.sum with |2 -> yield 1 | _ -> ()
}

map |> findXMAS |> Seq.sum |> printfn "Part 2: %A"