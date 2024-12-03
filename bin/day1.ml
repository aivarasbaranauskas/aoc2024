let file = "inputs/day1/input.txt"

let split_by_three_spaces line =
  let spl = String.split_on_char ' ' line in
  (List.nth spl 0, List.nth spl 3)

let read_input () =
  let lines = Shared.File.read_lines file in
  let a, b = List.split @@ List.map split_by_three_spaces lines in
  ( Shared.L.sort @@ List.map int_of_string a,
    Shared.L.sort @@ List.map int_of_string b )

let part1 () =
  let a, b = read_input () in
  let joined = List.combine a b in
  let diffed = List.map (fun (a, b) -> abs (a - b)) joined in
  Shared.L.sum diffed

let map_acc acc i =
  match acc with
  | [] -> [ (i, 1) ]
  | (a, ct) :: t -> if a == i then (i, ct + 1) :: t else (i, 1) :: (a, ct) :: t

let part2 () =
  let a, b = read_input () in
  let b_map = List.fold_left map_acc [] b in
  let multiply_by_freq a =
    match List.assoc_opt a b_map with None -> a * 0 | Some b -> a * b
  in
  let frequeced = List.map multiply_by_freq a in
  Shared.L.sum frequeced

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
