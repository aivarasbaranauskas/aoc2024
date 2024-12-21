let file = "inputs/day21/input.txt"

let read_input () =
  List.map Shared.S.explode_to_list @@ Shared.F.read_lines file

let direction_to_char d =
  match d with
  | 1, 0 -> 'v'
  | 0, 1 -> '>'
  | -1, 0 -> '^'
  | 0, -1 -> '<'
  | _ -> raise Not_found

(* 
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+*)
let gen_numeric_keypad start finish =
  let keypad_coords button =
    match button with
    | '7' -> (0, 0)
    | '8' -> (0, 1)
    | '9' -> (0, 2)
    | '4' -> (1, 0)
    | '5' -> (1, 1)
    | '6' -> (1, 2)
    | '1' -> (2, 0)
    | '2' -> (2, 1)
    | '3' -> (2, 2)
    | '0' -> (3, 1)
    | 'A' -> (3, 2)
    | _ -> raise Not_found
  in
  let start, finish = (keypad_coords start, keypad_coords finish) in
  let dist = Shared.Sets.IntPair.(rem start finish |> length) in
  let direction = Shared.Sets.IntPair.(rem finish start |> norm) in
  let banned_key = (3, 0) in
  let rec gen current path =
    if current = banned_key then []
    else if current = finish then
      [ List.map direction_to_char @@ List.rev path ]
    else if List.length path >= dist then []
    else
      let cx, cy = direction in
      List.append
        (gen (Shared.Sets.IntPair.add current (cx, 0)) ((cx, 0) :: path))
        (gen (Shared.Sets.IntPair.add current (0, cy)) ((0, cy) :: path))
  in
  gen start []

(* 
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+*)
let gen_directional_keypad start finish =
  let keypad_coords button =
    match button with
    | '^' -> (0, 1)
    | 'A' -> (0, 2)
    | '<' -> (1, 0)
    | 'v' -> (1, 1)
    | '>' -> (1, 2)
    | _ -> raise Not_found
  in
  let start, finish = (keypad_coords start, keypad_coords finish) in
  let dist = Shared.Sets.IntPair.(rem start finish |> length) in
  let direction = Shared.Sets.IntPair.(rem finish start |> norm) in
  let banned_key = (0, 0) in
  let rec gen current path =
    if current = banned_key then []
    else if current = finish then
      [ List.map direction_to_char @@ List.rev path ]
    else if List.length path >= dist then []
    else
      let cx, cy = direction in
      List.append
        (gen (Shared.Sets.IntPair.add current (cx, 0)) ((cx, 0) :: path))
        (gen (Shared.Sets.IntPair.add current (0, cy)) ((0, cy) :: path))
  in
  gen start []

let code_num code =
  let rec iter acc = function
    | [] | [ 'A' ] -> acc
    | x :: t ->
        let acc = (acc * 10) + (int_of_char x - int_of_char '0') in
        iter acc t
  in
  iter 0 code

module CCI = struct
  type t = char * char * int

  let compare (a0, b0, c0) (a1, b1, c1) =
    match Stdlib.compare a0 a1 with
    | 0 -> (
        match Stdlib.compare b0 b1 with 0 -> Stdlib.compare c0 c1 | x -> x)
    | x -> x
end

module CCIMap = Map.Make (CCI)

let mem = ref CCIMap.empty

let rec min_path_directional_len depth seq =
  let path_len_f =
    if depth = 1 then List.length else min_path_directional_len (depth - 1)
  in
  let rec iter acc = function
    | [] | [ _ ] -> acc
    | a :: f :: tl ->
        let min_path =
          match CCIMap.find_opt (a, f, depth) !mem with
          | Some x -> x
          | None ->
              let x =
                Shared.L.min
                @@ List.map (fun p -> path_len_f (p @ [ 'A' ]))
                @@ gen_directional_keypad a f
              in
              mem := CCIMap.add (a, f, depth) x !mem;
              x
        in
        iter (acc + min_path) (f :: tl)
  in
  iter 0 ('A' :: seq)

let min_path_numpad_len num_dir_keypads code =
  let rec iter acc = function
    | [] | [ _ ] -> acc
    | a :: f :: tl ->
        let paths =
          List.map (fun p ->
              min_path_directional_len num_dir_keypads (p @ [ 'A' ]))
          @@ gen_numeric_keypad a f
        in
        let acc = acc + Shared.L.min paths in
        iter acc (f :: tl)
  in
  iter 0 ('A' :: code)

let get_sum_complexities num_dir_keypads =
  let codes = read_input () in
  let calc_code_complexity code =
    let min_l = min_path_numpad_len num_dir_keypads code in
    let num = code_num code in
    min_l * num
  in
  let rec iter_codes acc = function
    | [] -> acc
    | code :: tl -> iter_codes (calc_code_complexity code + acc) tl
  in
  iter_codes 0 codes

let part1 () = get_sum_complexities 2
let part2 () = get_sum_complexities 25
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
