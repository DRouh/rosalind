let str1 = "ABCXYZAYABCXYZAY"
let str2 = "X"

let diag maxI maxJ (i, j) =
  let boundary = min (maxI - i) (maxJ - j) - 1
  [0..boundary] |> List.map (fun x -> (i + x, j + x))

let trimEndOfDiag (arr:int[,]) =
  List.choose (fun (i, j) -> if arr.[i, j] <> 0 then Some (i, j) else None)

let update a i j v = Array2D.set a (j + 1) (i + 1) v

let toList (arr: 'a [,]) = arr |> Seq.cast<'a> |> Seq.toList
let dec i = i - 1

let commonMotif (str1:string) (str2:string) =
  let maxI = str2.Length + 1
  let maxJ = str1.Length + 1
  let arr = Array2D.zeroCreate maxI maxJ

  let update' = update arr
  let diag' = diag maxI maxJ
  let trimEndOfDiag' = trimEndOfDiag arr
  let select' i = str1.[i]
  let stringByIndexes =
    trimEndOfDiag' >> List.map (snd >> dec >> select') >> System.String.Concat

  let startOfSubstring i j v = if v = 1 then Some (i, j) else None

  str1
  |> Seq.iteri
    (fun j c1 ->
      str2 |>
      Seq.iteri
        (fun i c2 -> update' j i (if c1 = c2 then arr.[i, j] + 1 else 0)))

  let indexes =
    arr
    |> Array2D.mapi startOfSubstring
    |> toList
    |> List.choose id

  indexes
  |> List.map (diag' >> stringByIndexes)
  |> List.distinct
  |> List.sortByDescending (fun x -> x.Length)

commonMotif str1 str2