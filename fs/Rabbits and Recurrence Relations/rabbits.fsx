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
              | 1   -> 1I
              | 2   -> 1I
              | gen -> g (gen - 1) + bigint(productionRate:int) * g (gen - 2)
  g gen

let gen = 32
let productionRate = 2
let grew = grow gen productionRate