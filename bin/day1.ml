let file = "inputs/day1/input.txt"

let split_by_three_spaces line =
  let spl = String.split_on_char ' ' line in
  (List.nth spl 0, List.nth spl 3)

let split_into_columns lines =
  List.split @@ List.map split_by_three_spaces lines

let sort l = List.sort Stdlib.compare l

let map_acc acc i =
  match acc with
  | [] -> [ (i, 1) ]
  | (a, ct) :: t -> if a == i then (i, ct + 1) :: t else (i, 1) :: (a, ct) :: t

let () =
  let lines = Shared.File.read_lines file in
  let a, b = split_into_columns lines in
  let a_int, b_int =
    (sort @@ List.map int_of_string a, sort @@ List.map int_of_string b)
  in
  let joined = List.combine a_int b_int in
  let diffed = List.map (fun (a, b) -> abs (a - b)) joined in
  let result_pt1 = List.fold_left (fun i acc -> i + acc) 0 diffed in

  let b_map = List.fold_left map_acc [] b_int in
  let distanced =
    List.map
      (fun a ->
        match List.assoc_opt a b_map with None -> a * 0 | Some b -> a * b)
      a_int
  in
  let result_pt2 = List.fold_left (fun i acc -> i + acc) 0 distanced in

  Printf.printf "Part 1: %d\nPart 2: %d\n" result_pt1 result_pt2
