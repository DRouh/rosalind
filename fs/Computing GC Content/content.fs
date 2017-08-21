let input = System.IO.File.ReadAllText("rosalind_gc.txt")

let mapSnd f (a, b) = (a, f b)

let readFasta (x:string) =
  let sbs = x.Split([|"\n"; "\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
  sbs |> Seq.fold (fun acc (x:string) ->
                      match (acc, x) with
                      | (xs, y)  when y.StartsWith(">") -> (y.Substring(1), "") :: xs
                      | ((r,rs)::xs, y) -> (r, rs + y) :: xs
                      | otherwise -> []
                  ) []

let computeGC x =
  let gc = x |> Seq.sumBy (fun c -> if c = 'G' || c = 'C' then 1 else 0)
  let all = x |> String.length
  100.0 * float(gc) / float(all)

let read = readFasta input
let r = read
      |> List.map (mapSnd computeGC)
      |> List.maxBy snd