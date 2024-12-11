let file = "inputs/day11/input.txt"

let read_input () =
  List.map int_of_string @@ String.split_on_char ' ' @@ Shared.F.read_all file

let split_if_even_digits_count x =
  let s = string_of_int x in
  if String.length s mod 2 = 0 then
    let half_length = String.length s / 2 in
    Some
      ( int_of_string @@ String.sub s 0 half_length,
        int_of_string @@ String.sub s half_length half_length )
  else None

module PairMap = Map.Make (Shared.Sets.IntPair)

let blink times =
  let mem = ref PairMap.empty in

  let rec expand x n =
    if n = 0 then 1
    else
      match PairMap.find_opt (x, n) !mem with
      | Some ct -> ct
      | None ->
          let ct =
            if x = 0 then expand 1 (n - 1)
            else
              match split_if_even_digits_count x with
              | Some (a, b) -> expand a (n - 1) + expand b (n - 1)
              | None -> expand (x * 2024) (n - 1)
          in
          mem := PairMap.add (x, n) ct !mem;
          ct
  in
  List.fold_left (fun acc x -> acc + expand x times) 0 @@ read_input ()

let part1 () = blink 25
let part2 () = blink 75
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
