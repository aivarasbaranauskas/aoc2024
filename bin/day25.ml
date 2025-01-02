let file = "inputs/day25/input.txt"

let read_input () =
  let lines = Shared.F.read_lines file in
  let grouped =
    List.fold_left
      (fun acc line ->
        match acc with
        | [] -> [ [ line ] ]
        | h :: t ->
            if List.length h < 7 then (line :: h) :: t
            else [] :: List.rev h :: t)
      [] lines
  in
  let grouped = (List.rev @@ List.hd grouped) :: List.tl grouped in
  List.rev grouped

let is_lock pattern = (List.hd pattern).[0] = '#'

let transform_lock pattern =
  let hs = Array.make 5 (-1) in
  List.iteri
    (fun linieNo line ->
      for i = 0 to 4 do
        if hs.(i) = -1 && line.[i] = '.' then hs.(i) <- linieNo - 1
      done)
    pattern;
  (hs.(0), hs.(1), hs.(2), hs.(3), hs.(4))

let transform_key pattern =
  let hs = Array.make 5 (-1) in
  List.iteri
    (fun linieNo line ->
      for i = 0 to 4 do
        if hs.(i) = -1 && line.[i] = '#' then hs.(i) <- 6 - linieNo
      done)
    pattern;
  (hs.(0), hs.(1), hs.(2), hs.(3), hs.(4))

module IntPent = struct
  type t = int * int * int * int * int

  let compare (a0, b0, c0, d0, e0) (a1, b1, c1, d1, e1) =
    match Stdlib.compare a0 a1 with
    | 0 -> (
        match Stdlib.compare b0 b1 with
        | 0 -> (
            match Stdlib.compare c0 c1 with
            | 0 -> (
                match Stdlib.compare d0 d1 with
                | 0 -> Stdlib.compare e0 e1
                | x -> x)
            | x -> x)
        | x -> x)
    | x -> x

  let add (a0, b0, c0, d0, e0) (a1, b1, c1, d1, e1) =
    (a0 + a1, b0 + b1, c0 + c1, d0 + d1, e0 + e1)
end

module PentSet = Set.Make (IntPent)

let part1 () =
  let grouped = read_input () in
  let locks, keys = List.partition is_lock grouped in
  let locks = List.map transform_lock locks in
  let keys = List.map transform_key keys in
  List.fold_left
    (fun acc lock ->
      let filtered =
        List.filter
          (fun key ->
            let a, b, c, d, e = (IntPent.add key lock) in
            a<=5 && b <=5 && c <= 5 && d <= 5 && e <= 5)
          keys
      in
      acc + List.length filtered)
    0 locks


let () = Printf.printf "%d\n" (part1 ())
