open System.Collections.Generic

let memoize f =
  let cache = Dictionary()
  fun x ->
    match cache.TryGetValue(x) with
    | true, v -> v
    | _ -> 
      let v = f x
      cache.Add(x, v)
      v

let grow gen productionRate = 
  let rec g = memoize <| 
              function
              | (1, pr:int) -> 1I
              | (2, pr)     -> 1I
              | (gen, pr)   -> g (gen - 1, pr) + bigint(pr) * g (gen - 2, pr) 
  g(gen, productionRate)        
let gen = 32
let productionRate = 2
let grew' = grow gen productionRate