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

let grow gen m = 
  let rec g = memoize <| 
              function
              | 0                -> 0I
              | 1                -> 1I
              | x when x <= m    -> g (x - 1) + g (x - 2)
              | x when x = m + 1 -> g (x - 1) + g (x - 2) - 1I
              | x                -> g (x - 1) + g (x - 2) - g (x - m - 1)
  g gen

let gen = 87
let m = 20
let grew = grow gen m