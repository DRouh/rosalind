#load "../codon.fsx"
open Codon
open System.IO

let reverseCodonTable: Map<AminoAcid, RnaCodon list> = 
  codonTable 
  |> Map.fold (fun m key value -> 
    match m.TryFind(value) with 
    | None -> m.Add(value, [key])
    | Some v -> m.Add(value, v @ [key])) Map.empty
    
let countReverseTranslations acids = 
  let freq = reverseCodonTable |> Map.map (fun _ v -> List.length v)
  let lookup s = match freq.TryFind(s) with | None -> 0 | Some v -> v
  in [lookup "Stop"] @ List.map (lookup) acids |> List.fold (fun state c -> state * c % 1000000) 1

let input = File.ReadAllText("rosalind_mrna.txt").Trim() |> List.ofSeq |> List.map (fun x -> x.ToString())
let translationsCount = input |> countReverseTranslations