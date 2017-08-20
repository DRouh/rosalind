let input = System.IO.File.ReadAllText("rosalind_dna.txt")
let counted = input 
              |> (fun x -> x.Trim())
              |> Seq.toList
              |> List.countBy id
              |> List.sortBy fst
              |> List.map snd