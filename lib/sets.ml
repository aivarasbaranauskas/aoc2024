module IntPair = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c

  let add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)
  let rem (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)
  let mul (x0, y0) (x1, y1) = (x0 * x1, y0 * y1)
  let dic (x0, y0) (x1, y1) = (x0 / x1, y0 / y1)
end

module PairsSet = Set.Make (IntPair)

module IntTriple = struct
  type t = int * int * int

  let compare (a0, b0, c0) (a1, b1, c1) =
    match Stdlib.compare a0 a1 with
    | 0 -> (
        match Stdlib.compare b0 b1 with 0 -> Stdlib.compare c0 c1 | x -> x)
    | x -> x

  let add (a0, b0, c0) (a1, b1, c1) = (a0 + a1, b0 + b1, c0 + c1)
  let rem (a0, b0, c0) (a1, b1, c1) = (a0 - a1, b0 - b1, c0 - c1)
  let mul (a0, b0, c0) (a1, b1, c1) = (a0 * a1, b0 * b1, c0 * c1)
  let dic (a0, b0, c0) (a1, b1, c1) = (a0 / a1, b0 / b1, c0 / c1)
end

module TriplesSet = Set.Make (IntTriple)

module IntQuad = struct
  type t = int * int * int * int

  let compare (a0, b0, c0, d0) (a1, b1, c1, d1) =
    match Stdlib.compare a0 a1 with
    | 0 -> (
        match Stdlib.compare b0 b1 with
        | 0 -> (
            match Stdlib.compare c0 c1 with 0 -> Stdlib.compare d0 d1 | x -> x)
        | x -> x)
    | x -> x

  let add (a0, b0, c0, d0) (a1, b1, c1, d1) =
    (a0 + a1, b0 + b1, c0 + c1, d0 + d1)

  let rem (a0, b0, c0, d0) (a1, b1, c1, d1) =
    (a0 - a1, b0 - b1, c0 - c1, d0 - d1)

  let mul (a0, b0, c0, d0) (a1, b1, c1, d1) =
    (a0 * a1, b0 * b1, c0 * c1, d0 * d1)

  let dic (a0, b0, c0, d0) (a1, b1, c1, d1) =
    (a0 / a1, b0 / b1, c0 / c1, d0 / d1)
end

module QuadsSet = Set.Make (IntQuad)

module IntPairPair = struct
  type t = (int * int) * (int * int)

  let compare (x0, y0) (x1, y1) =
    match IntPair.compare x0 x1 with 0 -> IntPair.compare y0 y1 | c -> c

  let add (x0, y0) (x1, y1) = (IntPair.add x0 x1, IntPair.add y0 y1)
  let rem (x0, y0) (x1, y1) = (IntPair.rem x0 x1, IntPair.rem y0 y1)
  let mul (x0, y0) (x1, y1) = (IntPair.mul x0 x1, IntPair.mul y0 y1)
  let dic (x0, y0) (x1, y1) = (IntPair.dic x0 x1, IntPair.dic y0 y1)
end

module PairPairsSet = Set.Make (IntPairPair)
