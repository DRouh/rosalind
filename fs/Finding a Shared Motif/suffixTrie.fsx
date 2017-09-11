let flip f a b = f b a
let const' a b = a
let ($) f a = f a
let init: 'a list -> 'a list =
  let rec loop ra = function
    | [] -> failwith "empty list"
    | x::xs ->
      match xs with
      | [] -> List.rev ra
      | _ -> loop (x::ra) xs
  in loop []

let rec tails = function
 | []-> [[]]
 | _::xs as x -> x :: tails xs

type SuffixTrie =
  | Leaf of int
  | Node of Map<Option<char>, SuffixTrie>

let empty = Map.empty
let insertWith f key value map =
    let newValue =
        match Map.tryFind key map with
        | Some old -> f value old
        | None -> value
    in Map.add key newValue map

let buildTrie (s:string) =
  let ts = s |> List.ofSeq |> tails |> init
  let len = s.Length

  let loop c run =
    let (<+>) _ (Node ns) = Node (run ns)
    in insertWith (<+>) (Some c) (Node $ run empty)

  let go run chars i (Node ns) =
    let tr =
      let addLeaf = Map.add None $ Leaf (i - 1)
      in List.foldBack loop chars addLeaf $ ns
    in run (i - 1) (Node $ tr)

  in List.fold go (flip const') ts len $ Node empty

buildTrie "bana"