open System.IO

// let file = "example.txt" // part 1: 143, part 2: 123
let file = "input.txt" // part 1: 5713, part 2: 5180 

let data = file |> File.ReadAllLines
let por = Array.sub data 0 (data |> Array.findIndex (fun x -> x = "")) |> Array.map (fun x -> x.Split("|") |> (fun a -> (int a[0], int a[1]))) |> Array.sortBy fst |> Array.toList
let pied = Array.sub data ((data |> Array.findIndex (fun x -> x = "")) + 1) (data.Length - (data |> Array.findIndex (fun x -> x = "")) - 1) |> Array.map (fun x -> x.Split(",") |> Array.map int |> Array.rev)
let pageInOrder currentPage l =
    let rulesForCurrentPageAndEarlierPage earlierPage = por |> List.filter (fun x -> x |> fst = currentPage && x |> snd = earlierPage)
    l |> Array.fold (fun resultSoFar earlierPage -> resultSoFar @ rulesForCurrentPageAndEarlierPage earlierPage) []
let rec pagesInOrder (l: int array) =
    match l with
    | _ when l.Length = 1 -> []
    | _ -> pageInOrder (l |> Array.head) (l |> Array.tail) |> (fun f -> f @ (pagesInOrder (l |> Array.tail)))
    
pied |> Array.filter (fun l -> (pagesInOrder l) = []) |> Array.map (fun l -> l[l.Length / 2]) |> Array.sum |> printfn "Part 1: %A"

let rec fixOrder a =
    let failingRules = pagesInOrder a
    match failingRules with
    | [] -> a
    | _ ->
        let firstIndex = Array.findIndex (fun x -> x = (failingRules.Head |> fst)) a
        let secondIndex = Array.findIndex (fun x -> x = (failingRules.Head |> snd)) a
        let x = a[firstIndex]
        a[firstIndex] <- a[secondIndex]
        a[secondIndex] <- x
        fixOrder a

pied |> Array.filter (fun l1 -> (pagesInOrder l1) <> []) |> Array.map (fun l2 -> fixOrder l2) |> Array.map (fun l -> l[l.Length / 2]) |> Array.sum |> printfn "Part 2: %A"
