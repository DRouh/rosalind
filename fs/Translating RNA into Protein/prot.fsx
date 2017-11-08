#load "../codon.fsx"
#load "../lib.fsx"
open System
open Codon
open Lib

let mapRnaToCodonString:seq<string> -> string list option =
    Seq.map (mapRnaCodon)
    >> Seq.takeWhile (function Some "Stop" -> false | _ -> true)
    >> List.ofSeq
    >> sequence

let input = System.IO.File.ReadAllText("rosalind_prot.txt")

let triples = input
            |> Array.ofSeq
            |> Array.chunkBySize 3
            |> Array.map String

let codon = triples
          |> mapRnaToCodonString
          |> Option.map (List.reduce (+))

System.IO.File.WriteAllText("output.txt", getOrElse codon "")