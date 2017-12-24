module ProteinFunctions
  #load "lib.fsx"
  #load "codon.fsx"
  open Codon
  open Lib

  type Protein = Protein of string
  type Dna = Dna of string
  type Rna = Rna of string

  module Dna =
    let create =
      Dna

    let get (Dna s) =
      s

    let map (f:string->string) =
      get >> f >> create

    let bind (f:string->Dna) =
      get >> f

    let (>>=) = bind

    let (>=>) f1 f2 =
      f1 >> bind f2

  module Rna =
    let create =
      Rna

    let get (Rna s) =
      s

    let map (f:string->string)  =
      get >> f >> create

    let bind (f:string->Rna) =
      get >> f

    let (>>=) = bind

    let (>=>) f1 f2 =
      f1 >> bind f2

    let fromDna = fun (Dna s) -> create (s.Replace("T", "U"))

  module Protein =
    let create =
      Protein

    let get (Protein s) =
      s

    let map (f:string->string) =
      get >> f >> create

    let bind (f:string->Protein) =
      get >> f

    let (>>=) = bind

    let (>=>) f1 f2 =
      f1 >> bind f2

    let rnaToProtein: string[] -> AminoAcid list option =
        Seq.map (mapRnaCodon)
        >> Seq.takeWhile (function Some "Stop" -> false | _ -> true)
        >> List.ofSeq
        >> sequence

    let fromRna: Rna -> Protein option =
      (function Rna s -> s)
      >> triples
      >> rnaToProtein
      >> Option.map (List.fold (+) "" >> create)
    let fromDna: Dna -> Protein option =
      Rna.fromDna >> fromRna