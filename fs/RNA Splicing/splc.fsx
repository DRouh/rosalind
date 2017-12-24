#load "../fasta.fsx"
#load "../protein.fsx"

open Fasta
open System.IO
open ProteinFunctions

let excludeIntron (dnaString:string) (intron:string) =
  dnaString.Replace(intron, "")

let (dnaString :: introns) =
  File.ReadAllText("rosalind_splc.txt")
  |> readFasta
  |> List.rev
  |> List.map snd

let protein =
  dnaString
  |> Dna.create
  |> Dna.map (fun s -> List.fold excludeIntron s introns)
  |> Protein.fromDna

let output =
  protein
  |> Option.map Protein.get
  |> sprintf "%A"

System.IO.File.WriteAllText("out.txt", output)