let expected (population:int[]) = 
  [float population.[0] * 1.0;
   float population.[1] * 1.0;
   float population.[2] * 1.0;
   float population.[3] * 0.75;
   float population.[4] * 0.5;
   float population.[5] * 0.0;
  ]
  |> List.sum
  |> (fun x -> x * 2.0)

let input = 
  System.IO.File.ReadAllText("rosalind_iev.txt") 
  |> (fun x -> x.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries))
  |> Array.map int

let e = expected input