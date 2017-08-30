#load "../fasta.fsx"
open Fasta

let toMatrix = List.map(List.ofSeq)

let rec transposeMatrix (x:'a list list) :'a list list = 
  match x |> List.filter (List.isEmpty >> not) with
  | [] -> []
  | xs -> (List.map List.head xs) :: transposeMatrix (List.map List.tail xs)

let sum = 
  let plus (a, b, c, d) (x, y, z, w) = 
    (a + x, b + y, c + z, d + w)
  let tuplify = function 
                | 'A' -> (1, 0, 0, 0)
                | 'C' -> (0, 1, 0, 0) 
                | 'G' -> (0, 0, 1, 0) 
                | 'T' -> (0, 0, 0, 1)
  let tupleToList = function (a, c, g, t) -> [a; c; g; t]   

  List.map tuplify
  >> List.fold plus (0, 0, 0, 0)
  >> tupleToList

let consensus = 
  let byIndex i = [|'A'; 'C'; 'G'; 'T'|].[i]

  List.map (List.indexed >> List.maxBy snd >> fst >> byIndex)
  >> System.String.Concat

let input = System.IO.File.ReadAllText("rosalind_gc.txt")
let fasta = input 
          |> readFasta 
          |> List.sortBy (fst) 
          |> List.map snd
let matrix = fasta 
           |> toMatrix 
           |> transposeMatrix
           |> List.map sum
let con = consensus matrix

System.IO.File.WriteAllText("output.txt", con)