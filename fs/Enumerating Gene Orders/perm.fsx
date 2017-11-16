open System
let permutations n =
  (*inserts v into every positions of the provided list*)
  let insertIntoEvery v =
    let rec insert prev acc = function
      | [] -> (prev @ [v]) :: acc
      | x::xs as l ->
        let preceding = prev @ [x]
        let accumulated = (prev @ [v] @ l) :: acc
        in insert preceding accumulated xs
    in (insert [] []) >> List.map (List.rev)

  (*starting from the list of a single element inserts elements in every position*)
  let rec permutations' = function
    | [] -> []
    | [x] -> [[x]]
    | v::xs ->
      permutations' (xs)
      |> List.fold (fun acc l -> acc @ insertIntoEvery v l) []

  in permutations' [1..n]

let permutationsFixedHead n =
  let remove v l = List.filter ((<>) v) l
  let rec permutations' = function
    | [] -> []
    | [v] -> [[v]]
    | l ->
      let go acc v =
        acc @ (l |> remove v |> permutations' |> List.map (fun p -> v::p))
      in List.fold go [] l
  in permutations' [1..n]

let formatOutput ps count =
  let start = sprintf "%i" count
  let go acc (x: int list) = sprintf "%s\n%s" acc (String.Join(" ", x))
  in ps |> List.fold go start

let ps = permutations 5

IO.File.WriteAllText("out.txt", formatOutput ps ps.Length)