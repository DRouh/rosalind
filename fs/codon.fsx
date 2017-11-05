module Codon

type AminoAcid = string
type RnaCodon = string

let mapRnaCodon: RnaCodon -> AminoAcid option = function
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

let codonTable : Map<RnaCodon, AminoAcid> = 
  ["UUU", "F"; "CUU", "L"; "AUU", "I"; "GUU", "V"; 
   "UUC", "F"; "CUC", "L"; "AUC", "I"; "GUC", "V"; 
   "UUA", "L"; "CUA", "L"; "AUA", "I"; "GUA", "V"; 
   "UUG", "L"; "CUG", "L"; "AUG", "M"; "GUG", "V"; 
   "UCU", "S"; "CCU", "P"; "ACU", "T"; "GCU", "A"; 
   "UCC", "S"; "CCC", "P"; "ACC", "T"; "GCC", "A"; 
   "UCA", "S"; "CCA", "P"; "ACA", "T"; "GCA", "A"; 
   "UCG", "S"; "CCG", "P"; "ACG", "T"; "GCG", "A"; 
   "UAU", "Y"; "CAU", "H"; "AAU", "N"; "GAU", "D"; 
   "UAC", "Y"; "CAC", "H"; "AAC", "N"; "GAC", "D"; 
   "UGU", "C"; "CGU", "R"; "AGU", "S"; "GGU", "G"; 
   "UGC", "C"; "CGC", "R"; "AGC", "S"; "GGC", "G"; 
   "UGG", "W"; "CGG", "R"; "AGG", "R"; "GGG", "G"
   "UAA", "Stop"; "CAA", "Q"; "AAA", "K"; "GAA", "E"; 
   "UAG", "Stop"; "CAG", "Q"; "AAG", "K"; "GAG", "E"; 
   "UGA", "Stop"; "CGA", "R"; "AGA", "R"; "GGA", "G"; 
  ] |> Map.ofList 