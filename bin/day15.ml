let file = "inputs/day15/input.txt"

let read_input () =
  let lines = Shared.F.read_lines file in
  let rec part map moves is_moves = function
    | [] ->
        ( Array.map Shared.S.explode_to_array @@ Array.of_list @@ List.rev map,
          List.flatten @@ List.map Shared.S.explode_to_list @@ List.rev moves )
    | x :: l ->
        if is_moves then part map (x :: moves) is_moves l
        else if x = "" then part map moves true l
        else part (x :: map) moves false l
  in
  part [] [] false lines

let print_map m (bot_r, bot_c) =
  let print_char r c v =
    print_char (if r = bot_r && c = bot_c then '@' else v)
  in
  let print_char_array r l =
    Array.iteri (print_char r) l;
    print_newline ()
  in
  Array.iteri print_char_array m;
  print_newline ()

let move_to_direction = function
  | '^' -> (-1, 0)
  | '>' -> (0, 1)
  | 'v' -> (1, 0)
  | '<' -> (0, -1)
  | _ -> raise Not_found

let find_bot map =
  let bot_r, bot_c = (ref (-1), ref (-1)) in
  Array.iteri
    (fun r line ->
      Array.iteri
        (fun c p ->
          if p = '@' then (
            bot_r := r;
            bot_c := c))
        line)
    map;
  map.(!bot_r).(!bot_c) <- '.';
  (map, (!bot_r, !bot_c))

let rec simulate map (bot_r, bot_c) = function
  | [] -> (map, (bot_r, bot_c))
  | move :: moves -> (
      let dr, dc = move_to_direction move in
      let next_r, next_c = (bot_r + dr, bot_c + dc) in
      match map.(next_r).(next_c) with
      | '#' -> simulate map (bot_r, bot_c) moves
      | '.' -> simulate map (next_r, next_c) moves
      | 'O' -> (
          let rec will_move nr nc =
            match map.(nr).(nc) with
            | 'O' -> will_move (nr + dr) (nc + dc)
            | '#' -> None
            | '.' -> Some (nr, nc)
            | _ -> raise (Failure "wtf 2")
          in
          match will_move next_r next_c with
          | None -> simulate map (bot_r, bot_c) moves
          | Some (nr, nc) ->
              map.(nr).(nc) <- 'O';
              map.(next_r).(next_c) <- '.';
              simulate map (next_r, next_c) moves)
      | _ -> raise (Failure "wtf"))

let rec simulate2 map (bot_r, bot_c) = function
  | [] -> (map, (bot_r, bot_c))
  | move :: moves -> (
      let dr, dc = move_to_direction move in
      let next_r, next_c = (bot_r + dr, bot_c + dc) in
      match map.(next_r).(next_c) with
      | '#' -> simulate2 map (bot_r, bot_c) moves
      | '.' -> simulate2 map (next_r, next_c) moves
      | '[' | ']' ->
          if dr = 0 then (
            (* move horizontal *)
            let rec will_move nc =
              match map.(next_r).(nc) with
              | '[' | ']' -> will_move (nc + dc + dc)
              | '#' -> None
              | '.' -> Some nc
              | _ -> raise (Failure "wtf 2")
            in
            match will_move next_c with
            | None -> simulate2 map (bot_r, bot_c) moves
            | Some nc ->
                let len = abs (next_c - nc) in
                let b, e =
                  if next_c < nc then (next_c, next_c + 1) else (nc + 1, nc)
                in
                Array.blit map.(next_r) b map.(next_r) e len;
                map.(next_r).(next_c) <- '.';
                simulate2 map (next_r, next_c) moves)
          else
            (* move vertical *)
            let rec can_move nr nc =
              match (map.(nr + dr).(nc), map.(nr + dr).(nc + 1)) with
              | '.', '.' -> true
              | '#', _ | _, '#' -> false
              | ']', '.' -> can_move (nr + dr) (nc - 1)
              | '.', '[' -> can_move (nr + dr) (nc + 1)
              | '[', ']' -> can_move (nr + dr) nc
              | ']', '[' ->
                  can_move (nr + dr) (nc - 1) && can_move (nr + dr) (nc + 1)
              | _, _ -> raise (Failure "wtf 3")
            in

            let rec do_move nr nc =
              (match (map.(nr + dr).(nc), map.(nr + dr).(nc + 1)) with
              | ']', '.' -> do_move (nr + dr) (nc - 1)
              | '.', '[' -> do_move (nr + dr) (nc + 1)
              | '[', ']' -> do_move (nr + dr) nc
              | ']', '[' ->
                  do_move (nr + dr) (nc - 1);
                  do_move (nr + dr) (nc + 1)
              | _, _ -> ());
              map.(nr + dr).(nc) <- '[';
              map.(nr + dr).(nc + 1) <- ']';
              map.(nr).(nc) <- '.';
              map.(nr).(nc + 1) <- '.'
            in

            let box_r, box_c =
              if map.(next_r).(next_c) = '[' then (next_r, next_c)
              else (next_r, next_c - 1)
            in

            if can_move box_r box_c then (
              do_move box_r box_c;
              simulate2 map (next_r, next_c) moves)
            else simulate2 map (bot_r, bot_c) moves
      | _ -> raise (Failure "wtf"))

let calculate_gps_coordinate_sum ch map =
  let sum = ref 0 in
  let sum_line r line =
    Array.iteri (fun c p -> if p = ch then sum := !sum + (100 * r) + c) line
  in
  Array.iteri sum_line map;
  !sum

let widen_map map =
  let rec widen_row acc = function
    | [] -> List.rev acc
    | h :: l -> (
        match h with
        | '@' -> widen_row ('.' :: '@' :: acc) l
        | 'O' -> widen_row (']' :: '[' :: acc) l
        | c -> widen_row (c :: c :: acc) l)
  in
  Array.map (fun row -> Array.of_list @@ widen_row [] @@ Array.to_list row) map

let part1 () =
  let map, moves = read_input () in
  let map, bot = find_bot map in
  let map, _ = simulate map bot moves in
  calculate_gps_coordinate_sum 'O' map

let part2 () =
  let map, moves = read_input () in
  let map = widen_map map in
  let map, bot = find_bot map in
  let map, bot = simulate2 map bot moves in
  print_map map bot;
  calculate_gps_coordinate_sum '[' map

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
