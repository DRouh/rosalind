#load "../fasta.fsx"
#load "../codon.fsx"
#load "../lib.fsx"
open Codon
open Fasta
open Lib

let complementDna =
              (fun (x:string) -> x.Trim())
              >> List.ofSeq
              >> List.rev
              >> List.map (function
                             'A' -> 'T'
                           | 'C' -> 'G'
                           | 'T' -> 'A'
                           | 'G' -> 'C'
                           | x -> x)
              >> Array.ofSeq
              >> System.String

let dnaCodonLookup = [
  ("TTT", "F"); ("CTT", "L"); ("ATT", "I"); ("GTT", "V");
  ("TTC", "F"); ("CTC", "L"); ("ATC", "I"); ("GTC", "V");
  ("TTA", "L"); ("CTA", "L"); ("ATA", "I"); ("GTA", "V");
  ("TTG", "L"); ("CTG", "L"); ("ATG", "M"); ("GTG", "V");
  ("TCT", "S"); ("CCT", "P"); ("ACT", "T"); ("GCT", "A");
  ("TCC", "S"); ("CCC", "P"); ("ACC", "T"); ("GCC", "A");
  ("TCA", "S"); ("CCA", "P"); ("ACA", "T"); ("GCA", "A");
  ("TCG", "S"); ("CCG", "P"); ("ACG", "T"); ("GCG", "A");
  ("TAT", "Y"); ("CAT", "H"); ("AAT", "N"); ("GAT", "D");
  ("TAC", "Y"); ("CAC", "H"); ("AAC", "N"); ("GAC", "D");
  ("CAA", "Q"); ("AAA", "K"); ("GAA", "E"); ("CAG", "Q");
  ("AAG", "K"); ("GAG", "E"); ("TGT", "C"); ("CGT", "R");
  ("AGT", "S"); ("GGT", "G"); ("TGC", "C"); ("CGC", "R");
  ("AGC", "S"); ("GGC", "G"); ("CGA", "R"); ("AGA", "R");
  ("GGA", "G"); ("TGG", "W"); ("CGG", "R"); ("AGG", "R");
  ("GGG", "G"); ("TGA", "Stop"); ("TAA", "Stop"); ("TAG", "Stop")] |> Map

let mapDnaCodon: RnaCodon -> AminoAcid option = flip Map.tryFind dnaCodonLookup

let possibleORFToProtein =
  let orfToProtein = triples >> Seq.map (mapDnaCodon) >> Seq.takeWhile (Option.isSome) >> List.ofSeq >> sequence
  let notStop = (<>) "Stop"
  in List.map (orfToProtein)
  >> List.choose id
  >> List.map (takeWhileInc notStop >> List.ofSeq)
  >> List.filter (fun x -> x |> List.tryLast = Some "Stop")
  >> List.map (List.takeWhile notStop >> (List.fold (+) ""))

let findPossibleORFs startDnaCodons dna  =
  startDnaCodons
  |> List.collect (fun (s:string) -> indexesOf s dna)
  |> List.filter (fun i -> i >= 0)
  |> List.sort
  |> List.distinct
  |> List.map (dna.Substring)

// main
let input = System.IO.File.ReadAllText("rosalind_orf.txt") |> readFasta |> List.head |> snd
let dna = input
let complementedDNA = input |> complementDna

let proteinFromDna = dna |> findPossibleORFs startDnaCodons |> possibleORFToProtein
let proteinsFromComplementedDna = complementedDNA |> findPossibleORFs startDnaCodons |> possibleORFToProtein
let proteins = proteinFromDna @ proteinsFromComplementedDna |> List.distinct

let result = proteins |> List.fold (fun acc c -> sprintf "%s\n%s" c acc) ""
System.IO.File.WriteAllText("result.txt", result)