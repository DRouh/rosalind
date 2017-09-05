#load "../fasta.fsx"
open Fasta

let input = System.IO.File.ReadAllText("rosalind_lcsm.txt")
let read = input |> readFasta |> List.map snd


type Key<'a> = 'a list

type Trie<'a> =
  Node of 'a option * Trie<'a> list * bool

type TrieList<'a> = Trie<'a> list

let empty : Trie<'a> list = []

let findKey (key:'a) (xs: TrieList<'a>) : Trie<'a> option =
  let byKey (Node (next, _, _)) = next = Some key
  xs |> List.tryFind byKey

let rec findTrie' (k:Key<'a>) (ts: TrieList<'a>): Trie<'a> option =
  let (>>=) a b = Option.bind b a
  in match (k, ts) with
     | ([], _) -> None
     | ([x], tries) -> findKey x tries
     | (x::xs, tries) ->
       let nextTrie xs (Node (_, next, _)) = findTrie' xs next
       in findKey x tries >>= nextTrie xs

let rec insert (k:Key<'a>) (ts:TrieList<'a>) : TrieList<'a> =
  match (k, ts) with
  | ([], t) -> t
  | (x::xs, tries) ->
    let isEndWord = xs = []
    let except v = tries |> List.filter ((<>) v)
    let toggleWordEnd old = isEndWord || old
    in
    match findKey x tries with
    | None -> (Node ((Some x), (insert xs []), isEndWord)) :: tries
    | Some value ->
      let (Node (key, next, word)) = value
      in (Node (key, (insert xs next), (toggleWordEnd word))) :: except value

