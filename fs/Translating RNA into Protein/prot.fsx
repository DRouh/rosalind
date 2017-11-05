#load "../codon.fsx"
open System
open Codon

let sequence xs =
    let f x acc = match x, acc with
                  | Some x, Some list -> Some (x :: list)
                  | _                 -> None
    List.foldBack f xs (Some [])

let mapRnaToCodonString:seq<string> -> string list option =
    Seq.map (mapRnaCodon)
    >> Seq.takeWhile (function Some "Stop" -> false | _ -> true)
    >> List.ofSeq
    >> sequence

let getOrElse o d = match o with Some v -> v | None -> d
let input = System.IO.File.ReadAllText("rosalind_prot.txt")

let triples = input
            |> Array.ofSeq
            |> Array.chunkBySize 3
            |> Array.map String

let codon = triples
          |> mapRnaToCodonString
          |> Option.map (List.reduce (+))

System.IO.File.WriteAllText("output.txt", getOrElse codon "")