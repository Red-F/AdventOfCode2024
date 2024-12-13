
open System.IO

// let file = "example.txt" // part 1: 1928, part 2: 2858
let file = "input.txt" // part 1: 6279058075753L, part 2: 6301361958738L
let data = file |> File.ReadAllText |> Seq.toList

let block i c = (match i % 2 with | 0 -> Array.create (int (c - '0')) (i / 2) | _ -> Array.create (int (c - '0')) -1) |> Array.toList
let blocks d = d |> List.mapi block |> List.filter (fun x -> x.Length > 0) |> List.fold (fun acc x -> acc @ x) [] |> List.toArray
let deFrag b = b |> Array.iteri (fun i n -> match n with | -1 -> (match (Array.findIndexBack (fun v -> v <> -1) b) with | x when x > i -> b[i] <- b[x]; b[x] <- -1 | _ -> ()) | _ -> ()); b
let checkSum b = b |> Array.mapi (fun i n -> match n with | -1 -> 0L | _ -> int64 i * (int64 n)) |> Array.sum
data |> blocks |> deFrag |> checkSum |> printfn "Part 1: %A"

let blockT i c = (match i % 2 with | 0 -> (i / 2), (int (c - '0'))  | _ -> -1, (int (c - '0')))
let blocksT d = d |> List.mapi blockT |> List.toArray
let fileIdIndex id (b: (int * int) array) = b |> Array.findIndex (fun (v,_) -> v = id)
let findFreeBlocksIndex (b: (int * int) array) fileIdIndex fs= Array.tryFindIndex (fun (v, size) -> v = -1 && fs <= size) b |> (fun i -> match i with | Some ix -> (match ix with | n when n < fileIdIndex -> Some ix | _ -> None) | None -> None)
let fileSize id (b: (int * int) array) = b[fileIdIndex id b] |> snd
let freeBlocks (b: (int * int) array) idIndex fs =  b[idIndex] <- (-1, fs); b
let useBlocks id fs freeBlocksIndex freeBlocksSize (b: (int * int) array) = b |> Array.removeAt freeBlocksIndex |> Array.insertManyAt freeBlocksIndex [(id,fs); (-1,freeBlocksSize-fs)]
let CheckSumT (b: (int * int ) array) = b |> Array.fold (fun (pos: int64, sum) (id, count) -> match int64 id with | -1L -> (pos + int64 count, sum) | _ -> (pos + int64 count, (seq { for i in pos .. pos + int64 count - 1L do yield i * int64 id } |> Seq.sum) + sum)) (0L, 0L)
let rec moveFile id b =
    match id with
    | -1 -> b
    | _ -> fileIdIndex id b |> (fun idIndex -> b[idIndex] |> snd |> (fun fs -> match findFreeBlocksIndex b idIndex fs with | Some fbIx -> (fbIx, b[fbIx] |> snd) |> (fun (freeBlocksIndex, freeBlocksSize) -> freeBlocks b idIndex fs |> useBlocks id fs freeBlocksIndex freeBlocksSize) | None -> b)) |> moveFile (id - 1) 

data |> blocksT |> (fun b -> moveFile (b[(b |> Array.length) - 1] |> fst) b) |> Array.filter (fun (_, n) -> n <> 0) |> CheckSumT |> snd |> printfn "Part 2: %A"
