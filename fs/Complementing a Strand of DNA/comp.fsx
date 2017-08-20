let input = System.IO.File.ReadAllText("rosalind_revc.txt")
let complement = input
              |> (fun x -> x.Trim())
              |> List.ofSeq
              |> List.rev
              |> List.map (function
                             'A' -> 'T'
                           | 'C' -> 'G'
                           | 'T' -> 'A'
                           | 'G' -> 'C'
                           | x -> x)
let output = complement |> System.String.Concat
System.IO.File.WriteAllText("output.txt", output)                       