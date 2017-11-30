#load "../fasta.fsx"
#load "../lib.fsx"
open System
open Fasta
open Lib

type StringGroup = {
  Length: int32
  SubGroup: SubGroup list
}
and SubGroup = {
  Position: int32
  String: string
}

let createStringGroup (length:int32) (xs: (int32 * string) list) =
  let sub = xs |> List.map (fun (p, s) -> { Position = p; String = s})
  in { Length = length; SubGroup = sub }

let complement =
  let comp = function 'A' -> 'T' | 'C' -> 'G' | 'T' -> 'A' | 'G' -> 'C' | x -> x
  in (fun (x:string) -> x.Trim())
  >> List.ofSeq
  >> List.rev
  >> List.map comp
  >> concatStr

let findEqualsInGroup (str:string) { Length = len; SubGroup = xs } =
  let compare { Position = pos; String = s } =
    if s = (str.Substring(str.Length - pos - len, len))
    then Some(pos, len)
    else None
  in xs |> List.choose compare

let findReversePalindromes dna lB uB =
  let allTails = List.ofSeq >> tails >> List.map concatStr
  let complementedDna = dna |> complement

  let chunks i s =
    let s' = s |> strToChars
    let createLengthGroup n =
      let subgroups = n
                      |> (flip Seq.chunkBySize) s'
                      |> Seq.mapi (fun j chs -> i + (j * n), concatStr chs)
                      |> Seq.takeWhile (fun (_, s) -> s.Length = n)
                      |> List.ofSeq
      in createStringGroup n subgroups
    let lowerBound = lB
    let upperBound = if s.Length - uB > 0 then uB + 1 else lowerBound
    in [lowerBound..upperBound] |> List.map createLengthGroup

  let groups = allTails >> List.takeWhile (fun x -> x.Length >= lB) >> List.mapi chunks >> List.collect id
  in dna |> groups |> List.collect (findEqualsInGroup complementedDna) |> List.distinct |> List.map (onFst inc) |> List.sortBy (id)

let input = IO.File.ReadAllText("rosalind_revp.txt") |> readFasta |> List.head |> snd
let result = findReversePalindromes input 4 12

let output = result |> List.map (fun (a,b) -> sprintf "%i %i" a b) |> List.fold (sprintf "%s\n%s") ""
do IO.File.WriteAllText("output.txt", output)