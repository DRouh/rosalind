#load "../fasta.fsx"
open Fasta

let isOverlap (n:string) (m:string) (k:int) =
  if k < m.Length then n.EndsWith(m.Substring(0, k))
  else false

let cartesian xs ys = 
  xs 
  |> List.collect(fun x -> ys |> List.map(fun y -> (y, x)))

let k = 3 // suffix length
let input = System.IO.File.ReadAllText("rosalind_grph.txt")

let fastaInput = readFasta input

let graph = 
  fastaInput 
  |> cartesian fastaInput
  |> List.filter (fun (a, b) -> (fst a <> fst b))
  |> List.filter (fun (a, b) -> isOverlap (snd a) (snd b) k)
  |> List.map (fun (x, y) -> fst x, fst y)

let str = graph |> List.map (fun (x, y) -> sprintf "%s %s \n" x y) |> System.String.Concat

System.IO.File.WriteAllText("output.txt", str)   