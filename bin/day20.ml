let file = "inputs/day20/input.txt"
let read_input () = Shared.F.read_char_matrix_2d_array file

let find_char ch m =
  let h, w = (Array.length m, Array.length m.(0)) in
  let rec iter r c =
    if c = w then iter (r + 1) 0
    else if r = h then None
    else if m.(r).(c) = ch then Some (r, c)
    else iter r (c + 1)
  in
  match iter 0 0 with None -> raise (Failure "not found") | Some p -> p

let get_from_map m (pr, pc) =
  try m.(pr).(pc)
  with Invalid_argument s ->
    raise (Invalid_argument (Printf.sprintf "%s at %d,%d" s pr pc))

let in_bounds (r, c) m =
  r >= 0 && c >= 0 && r < Array.length m && c < Array.length m.(0)

let collect_route m =
  let start = find_char 'S' m in
  let finish = find_char 'E' m in
  let mem = ref Shared.Sets.PairsMap.empty in
  let rec collect_route_rec acc pos =
    mem := Shared.Sets.PairsMap.add pos acc !mem;
    if Shared.Sets.IntPair.compare pos finish = 0 then ()
    else
      let down = Shared.Sets.IntPair.add pos (1, 0) in
      let right = Shared.Sets.IntPair.add pos (0, 1) in
      let up = Shared.Sets.IntPair.add pos (-1, 0) in
      let left = Shared.Sets.IntPair.add pos (0, -1) in
      if
        get_from_map m down != '#'
        && Shared.Sets.PairsMap.find_opt down !mem = None
      then collect_route_rec (acc + 1) down
      else if
        get_from_map m right != '#'
        && Shared.Sets.PairsMap.find_opt right !mem = None
      then collect_route_rec (acc + 1) right
      else if
        get_from_map m up != '#' && Shared.Sets.PairsMap.find_opt up !mem = None
      then collect_route_rec (acc + 1) up
      else if
        get_from_map m left != '#'
        && Shared.Sets.PairsMap.find_opt left !mem = None
      then collect_route_rec (acc + 1) left
      else
        raise
          (Failure
             (Printf.sprintf "uh oh at %s" (Shared.Sets.IntPair.to_string pos)))
  in
  collect_route_rec 0 start;
  !mem

let dist (r1, c1) (r2, c2) = (abs(r1-r2))+(abs(c1-c2))

let count_the_cheats cheat_lim cheat_len =
  let m = read_input () in
  let mem = collect_route m in

  let is_cheat cb ce =
    match Shared.Sets.PairsMap.find_opt ce mem with
    | None -> false
    | Some x ->
        let cheat_size = dist cb ce in
        let cheat_save = Shared.Sets.PairsMap.find cb mem - x - cheat_size in
        cheat_save >= cheat_lim
  in

  let count_cheats_in_pos (pr, pc) =
    let ct = ref 0 in
    for rd = 0-cheat_len to cheat_len do
      for cd = 0-cheat_len to cheat_len do
        let p = (abs rd) + (abs cd) in
        if p>1 && p <= cheat_len then
          let r = pr + rd in
          let c = pc + cd in
          if in_bounds (r, c) m && is_cheat (pr, pc) (r, c) then ct := !ct + 1
      done
    done;
    !ct
  in

  let rec count_cheats acc = function
    | [] -> acc
    | (h, _) :: t ->
        let cheats = count_cheats_in_pos h in
        count_cheats (acc + cheats) t
  in
  let path = Shared.Sets.PairsMap.to_list mem in
  count_cheats 0 path

let part1 () = count_the_cheats 100 2
let part2 () = count_the_cheats 100 20
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
