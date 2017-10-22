let (^) = pown
let (^^) = pown

let combination n k = List.fold (fun s i -> s * (n - i + 1) / i ) 1 [1..k]

let bernoulliTrial n i s f = 
  let combinations = combination n i |> double
  combinations * (s ^^ i) * (f ^^ (n - i))

let probabilityOfAtLeast gens num = 
  // 1.0 - sum(P(0),P(1),..P(num-1))
  let total = 2 ^ gens
  1.0 - List.sumBy (fun i -> bernoulliTrial total i 0.25 0.75) [0..num - 1]

let generations = 2
let number = 1

let probability = probabilityOfAtLeast generations number
sprintf "%f" probability