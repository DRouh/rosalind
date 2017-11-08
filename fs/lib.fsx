module Lib

open System.Text.RegularExpressions

let sequence xs =
    let f x acc = match x, acc with
                  | Some x, Some list -> Some (x :: list)
                  | _                 -> None
    List.foldBack f xs (Some [])

let getOrElse o d = match o with Some v -> v | None -> d

let flip f a b = f b a

let indexesOf p s =
  Regex.Matches(s, p)
  |> Seq.cast<Match>
  |> Seq.map (fun x -> x.Index)
  |> List.ofSeq

// inclusive version of takeWhile - includes the element which broke the condition
// http://www.fssnip.net/n8/title/Splitting-a-sequence-based-on-separator-condition
let takeWhileInc cond s =
  let notEmpty s = not (Seq.isEmpty s)
  in seq {
    yield! s |> Seq.takeWhile cond
    let r = s |> Seq.skipWhile cond
    if notEmpty r then yield r |> Seq.head
  }

let triples: string -> System.String [] =
  Array.ofSeq >> Array.chunkBySize 3 >> Array.map System.String