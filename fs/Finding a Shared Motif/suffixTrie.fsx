let flip f a b = f b a
let const' a b = a
let empty = Map.empty

let rec tails = function
 | []-> [[]]
 | _::xs as x -> x :: tails xs

type SuffixTrie =
  | Leaf of int
  | Node of MapSuffix
and MapSuffix = Map<Option<char>, SuffixTrie>

let insertWith f key value map =
    let newValue =
        match Map.tryFind key map with
        | Some(p) -> f value p
        | None    -> value
    in Map.add key newValue map

let buildTrie (s:string) : SuffixTrie =
  let ts = s |> List.ofSeq |> tails
  let len = s.Length

  let loop c run =
    let (<+>) _ (Node ns) = Node (run ns)
    in insertWith (<+>) (Some c) (Node (run empty))

  let go run chars i (Node ns) =
    let tr xs ns =
      let addLeaf = Map.add None (Leaf (i - 1))
      let insert' = List.foldBack loop xs addLeaf
      in insert' ns
    in run (i - 1) (Node (tr chars ns))
  let bb = flip const'
  in List.fold go bb ts len (Node empty)

buildTrie "banana"

//a = char
//b = (MapSuffix -> MapSuffix)
//(Char -> (MapSuffix -> MapSuffix) -> MapSuffix -> MapSuffix) -> (MapSuffix -> MapSuffix) -> t Char -> MapSuffix -> MapSuffix)
//foldr::(a -> b -> b) -> b -> [a] -> b)