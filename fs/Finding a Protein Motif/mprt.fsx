#load "../fasta.fsx"
open Fasta
open System
open System.Net
open System.Text.RegularExpressions

let fetchUrlAsync (name,url) =
    async {
        let req = WebRequest.Create(Uri(url))
        use! resp = req.AsyncGetResponse()
        use stream = resp.GetResponseStream()
        use reader = new IO.StreamReader(stream)
        return (name, reader.ReadToEnd())
    }

let parsePattern : string -> Regex =
  let processNot s =
    Regex.Replace(s, "\\{(.{1})\\}", fun (m:Match) -> sprintf "[^%s]" m.Groups.[1].Value)
  let processOr s =
    let matchEval (m:Match) =
      sprintf "[%s]" (String.Join("|", m.Groups.[1].Value.ToCharArray()))
    in Regex.Replace(s, "\\[(.+)\\]", matchEval)
  let processOverlapping = sprintf "(?=%s)([A-Z]{1})"
  in processOr >> processNot >> processOverlapping >> Regex

let proteinsFasta =
  IO.File.ReadAllLines("mprt.txt")
  |> Array.map ((fun s -> (s, sprintf "http://www.uniprot.org/uniprot/%s.fasta" s)) >> fetchUrlAsync)
  |> Async.Parallel
  |> Async.RunSynchronously
  |> List.ofArray
  |> List.map (fun (n,v) -> (n, (v |> readFasta).[0] |> snd))

let patternOccurences (p:Regex) (s:string) =
  p.Matches(s) |> Seq.cast |> Seq.map (fun (x:Match) -> x.Index + 1) |> Array.ofSeq

let patternOccurences' = "N{P}[ST]{P}" |> parsePattern  |> patternOccurences
let occurences =
  proteinsFasta
  |> Array.ofList
  |> Array.Parallel.map (fun (n, v) -> (n, patternOccurences' v))
  |> Array.filter (snd >> Array.isEmpty >> not)

let prettyPrint (v:(string*int []) []) =
  let formatOcc (i:int []) = String.Join(" ", i)
  let ss = v |> Array.map (fun (n, v) -> sprintf "%s\n%s\n" n (formatOcc v))
  System.String.Join("", ss)

IO.File.WriteAllText("1.txt", occurences |> prettyPrint)