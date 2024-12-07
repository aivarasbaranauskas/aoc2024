let file = "inputs/day6/input.txt"
let read_input () = Shared.F.read_char_matrix_2d_array file

let find_guard m =
  match
    Array.find_mapi
      (fun ir row ->
        match
          Array.find_mapi
            (fun ic x -> match x with '^' -> Some ic | _ -> None)
            row
        with
        | Some ic -> Some (ir, ic)
        | None -> None)
      m
  with
  | Some x -> x
  | None -> raise (Failure "guard not found")

module IntPair = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c

  let add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)
end

module PairsSet = Set.Make (IntPair)

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
end

module QuadsSet = Set.Make (IntQuad)

let next_direction current_direction =
  match current_direction with
  | -1, 0 -> (0, 1)
  | 0, 1 -> (1, 0)
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | _ -> raise (Failure "bad direction")

let part1 () =
  let m = read_input () in
  let guard_row, guard_col = find_guard m in
  Array.set m.(guard_row) guard_col '.';
  let h, w = (Array.length m, Array.length m.(0)) in
  let rec walk guard direction visited =
    let next_row, next_col = IntPair.add guard direction in
    if next_row < 0 || next_row >= h || next_col < 0 || next_col >= w then
      PairsSet.add guard visited (* return all visited from here *)
    else
      match m.(next_row).(next_col) with
      | '.' ->
          let visited = PairsSet.add guard visited in
          walk (next_row, next_col) direction visited
      | '#' -> walk guard (next_direction direction) visited
      | _ -> raise (Failure "lost")
  in
  let visited = walk (guard_row, guard_col) (-1, 0) PairsSet.empty in
  List.length @@ PairsSet.elements visited

let does_make_a_loop m (obsticle_r, obsticle_c) (init_guard_row, init_guard_col)
    =
  let h, w = (Array.length m, Array.length m.(0)) in
  let rec walk (guard_row, guard_col) (direction_r, direction_c) visited =
    let next_row, next_col =
      (guard_row + direction_r, guard_col + direction_c)
    in
    if next_row < 0 || next_row >= h || next_col < 0 || next_col >= w then false
    else if next_row = obsticle_r && next_col = obsticle_c then
      walk (guard_row, guard_col)
        (next_direction (direction_r, direction_c))
        visited
    else
      match m.(next_row).(next_col) with
      | '.' ->
          if
            QuadsSet.exists
              (fun (a, b, c, d) ->
                guard_row = a && guard_col = b && direction_r = c
                && direction_c = d)
              visited
          then true
          else
            walk (next_row, next_col) (direction_r, direction_c)
              (QuadsSet.add
                 (guard_row, guard_col, direction_r, direction_c)
                 visited)
      | '#' ->
          walk (guard_row, guard_col)
            (next_direction (direction_r, direction_c))
            visited
      | _ -> raise (Failure "lost")
  in
  walk (init_guard_row, init_guard_col) (-1, 0) QuadsSet.empty

let part2 () =
  let m = read_input () in
  let init_guard_row, init_guard_col = find_guard m in
  Array.set m.(init_guard_row) init_guard_col '.';
  let h, w = (Array.length m, Array.length m.(0)) in
  let ct = ref 0 in
  let rec walk (guard_row, guard_col) (direction_r, direction_c) visited
      obstacle_options =
    let next_row, next_col =
      (guard_row + direction_r, guard_col + direction_c)
    in
    if next_row < 0 || next_row >= h || next_col < 0 || next_col >= w then
      obstacle_options
    else
      match m.(next_row).(next_col) with
      | '.' ->
          Printf.printf "%d: %d %d\n%!" !ct guard_row guard_col;
          ct := !ct + 1;

          let obstacle_options =
            if
              (next_row != init_guard_row || init_guard_col != next_col)
              && does_make_a_loop m (next_row, next_col)
                   (init_guard_row, init_guard_col)
            then PairsSet.add (next_row, next_col) obstacle_options
            else obstacle_options
          in

          walk (next_row, next_col) (direction_r, direction_c)
            (QuadsSet.add
               (guard_row, guard_col, direction_r, direction_c)
               visited)
            obstacle_options
      | '#' ->
          walk (guard_row, guard_col)
            (next_direction (direction_r, direction_c))
            visited obstacle_options
      | _ -> raise (Failure "lost")
  in
  let obstacle_options =
    walk (init_guard_row, init_guard_col) (-1, 0) QuadsSet.empty PairsSet.empty
  in

  List.length @@ PairsSet.elements obstacle_options

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
