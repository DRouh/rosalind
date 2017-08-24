open System
let sequence xs =
    let f x acc = match x, acc with
                  | Some x, Some list -> Some (x :: list)
                  | _                 -> None
    List.foldBack f xs (Some [])

let mapRnaCodon = function
                | "UAA" | "UAG" | "UGA" -> Some "Stop"
                | "GCU" | "GCC" | "GCA" | "GCG" ->  Some  "A"
                | "UGU" | "UGC" -> Some "C"
                | "GAU" | "GAC" -> Some "D"
                | "GAA" | "GAG" -> Some "E"
                | "UUU" | "UUC" -> Some "F"
                | "GGU" | "GGC" | "GGA" | "GGG" -> Some "G"
                | "CAU" | "CAC" -> Some "H"
                | "AUU" | "AUC" | "AUA" -> Some "I"
                | "AAA" | "AAG" -> Some "K"
                | "UUA" | "UUG" | "CUU" | "CUC" | "CUA" | "CUG" -> Some "L"
                | "ACU" | "ACC" | "ACA" | "ACG" -> Some "T"
                | "AAU" | "AAC" -> Some "N"
                | "AUG" -> Some "M"
                | "CCU" | "CCC" | "CCA" | "CCG" -> Some "P"
                | "CAA" | "CAG" -> Some "Q"
                | "CGU" | "CGC" | "CGA" | "CGG" | "AGA" | "AGG" -> Some "R"
                | "UCU" | "UCC"| "UCA"| "UCG" | "AGU" | "AGC" ->  Some "S"
                | "GUU" | "GUC" | "GUA" | "GUG" -> Some "V"
                | "UGG" -> Some "W"
                | "UAU" | "UAC" -> Some "Y"
                | _ -> None

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