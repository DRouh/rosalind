let k = 29 // homozygous dominant: XX
let m = 24 // heterozygous: Xx
let n = 30 // homozygous recessive: xx
let population = k + m + n

let prob (k:int) m n =
  let kf = float k
  let mf = float m
  let nf = float n
  let total = kf + mf + nf
  [
    2.00 * kf * mf;
    2.00 * kf * nf;
    1.00 * kf * (kf - 1.0);
    1.0 * mf * nf;
    0.75 * mf * (mf - 1.0)
  ] |> List.sum |> (fun x -> x / (total * (total - 1.0)))

prob k m n