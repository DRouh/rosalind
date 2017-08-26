let input = System.IO.File.ReadAllText("rosalind_subs.txt")

let [|str; motif|] = input.Split([|"\n"; "\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)

let subs (str:string) (sub:string) = 
  let sLen = str.Length
  let len = sub.Length
  let first = sub.[0]
  let last = sub.[len - 1]
  
  [0..sLen - 1] 
  |> List.filter (fun i -> str.[i] = first && (i + len < sLen) && (str.[i + len - 1] = last))
  |> List.map (fun i -> (i, str.Substring(i, len) = sub)) 
  |> List.filter (snd) 
  |> List.map (fst >> (fun x -> x + 1)) // +1 to conform to Rosalind expectations

let sbs = subs str motif