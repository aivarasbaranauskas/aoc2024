let file = "inputs/day14/input.txt"

let read_input () =
  List.map (fun line ->
      Scanf.sscanf line "p=%d,%d v=%d,%d" (fun a b c d -> ((a, b), (c, d))))
  @@ Shared.F.read_lines file

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

let simulate lim_x lim_y seconds ((pos_x, pos_y), (vec_x, vec_y)) =
  let pos_x = modulo (pos_x + (seconds * vec_x)) lim_x in
  let pos_y = modulo (pos_y + (seconds * vec_y)) lim_y in
  (pos_x, pos_y)

let part1 () =
  let bots = read_input () in
  let lim_x, lim_y, seconds = (101, 103, 100) in
  let simulated = List.map (simulate lim_x lim_y seconds) bots in
  let qx, qy = ((lim_x - 1) / 2, (lim_y - 1) / 2) in
  let q1, q2, q3, q4 =
    List.fold_left
      (fun (q1, q2, q3, q4) (x, y) ->
        if x < qx then
          if y < qy then (q1 + 1, q2, q3, q4)
          else if y > qy then (q1, q2 + 1, q3, q4)
          else (q1, q2, q3, q4)
        else if x > qx then
          if y < qy then (q1, q2, q3+1, q4)
          else if y > qy then (q1, q2, q3, q4+1)
          else (q1, q2, q3, q4)
        else (q1, q2, q3, q4))
      (0, 0, 0, 0) simulated
  in
  q1 * q2 * q3 * q4

module PairsMap = Map.Make (Shared.Sets.IntPair)

let part2 () = 
  let bots = read_input () in
  let lim_x, lim_y = 101, 103 in
  let rec draw_and_ask n =
    let bots_now = List.map (simulate lim_x lim_y n) bots in
    let grouped = List.fold_left (fun acc bot -> if PairsMap.mem bot acc then PairsMap.add bot (1 + PairsMap.find bot acc) acc else PairsMap.add bot 1 acc) PairsMap.empty bots_now in
    print_newline();
    for x =0 to lim_x-1 do
      for y=0 to lim_y-1 do
        match PairsMap.find_opt (x, y) grouped with
        | None -> print_char ' '
        | Some(z) -> print_char (char_of_int (z + int_of_char '0'))
      done;
      print_newline();
    done;
    print_newline();
    print_string "good? ->";
    let s = read_line () in
    if s = "y" then n else if s = "b" then draw_and_ask (n-1) else draw_and_ask (n+1)
  in
  draw_and_ask 1

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
