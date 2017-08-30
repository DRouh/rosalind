module Fasta

let readFasta (x:string) =
  x.Split([|"\n"; "\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
  |> Seq.fold (fun acc (x:string) ->
                   match (acc, x) with
                   | (xs, y)  when y.StartsWith(">") -> (y.Substring(1), "") :: xs
                   | ((r,rs)::xs, y) -> (r, rs + y) :: xs
                   | otherwise -> []) []