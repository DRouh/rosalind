let input = System.IO.File.ReadAllText("rosalind_hamm.txt")

let hammingDistance (a:string) (b:string) =
    a
    |> List.ofSeq
    |> List.zip (b |> List.ofSeq)
    |> List.sumBy (fun (a, b) -> if a <> b then 1 else 0)

let calcHammingDistance = function
    | [|a:string; b|] when a.Length = b.Length -> Some (hammingDistance a b)
    | otherwise -> None

let distance =
    input.Split([|"\n"; "\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    |> calcHammingDistance