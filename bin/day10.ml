let file = "inputs/day10/input.txt"

let read_input () =
  Shared.Matrix.map (fun x -> int_of_char x - int_of_char '0')
  @@ Shared.F.read_char_matrix_2d_array file

let part1 () =
  let m = read_input () in
  let r_max = Array.length m in
  let c_max = Array.length m.(0) in
  let rec find_all_trailtails r c =
    if m.(r).(c) = 9 then Shared.Sets.PairsSet.(empty |> add (r, c))
    else
      let right =
        if c + 1 < c_max && m.(r).(c) + 1 = m.(r).(c + 1) then
          find_all_trailtails r (c + 1)
        else Shared.Sets.PairsSet.empty
      in
      let left =
        if c - 1 >= 0 && m.(r).(c) + 1 = m.(r).(c - 1) then
          find_all_trailtails r (c - 1)
        else Shared.Sets.PairsSet.empty
      in
      let down =
        if r + 1 < r_max && m.(r).(c) + 1 = m.(r + 1).(c) then
          find_all_trailtails (r + 1) c
        else Shared.Sets.PairsSet.empty
      in
      let up =
        if r - 1 >= 0 && m.(r).(c) + 1 = m.(r - 1).(c) then
          find_all_trailtails (r - 1) c
        else Shared.Sets.PairsSet.empty
      in
      Shared.Sets.PairsSet.(union right left |> union up |> union down)
  in
  let rec check_all acc r c =
    if c >= c_max then check_all acc (r + 1) 0
    else if r >= r_max then acc
    else if m.(r).(c) = 0 then
      let score = Shared.Sets.PairsSet.cardinal @@ find_all_trailtails r c in
      check_all (acc + score) r (c + 1)
    else check_all acc r (c + 1)
  in
  check_all 0 0 0

let part2 () =
  let m = read_input () in
  let r_max = Array.length m in
  let c_max = Array.length m.(0) in
  let rec how_many_trails r c =
    if m.(r).(c) = 9 then 1
    else
      (if c + 1 < c_max && m.(r).(c) + 1 = m.(r).(c + 1) then
         how_many_trails r (c + 1)
       else 0)
      + (if c - 1 >= 0 && m.(r).(c) + 1 = m.(r).(c - 1) then
           how_many_trails r (c - 1)
         else 0)
      + (if r + 1 < r_max && m.(r).(c) + 1 = m.(r + 1).(c) then
           how_many_trails (r + 1) c
         else 0)
      +
      if r - 1 >= 0 && m.(r).(c) + 1 = m.(r - 1).(c) then
        how_many_trails (r - 1) c
      else 0
  in
  let rec check_all acc r c =
    if c >= c_max then check_all acc (r + 1) 0
    else if r >= r_max then acc
    else if m.(r).(c) = 0 then
      let score = how_many_trails r c in
      check_all (acc + score) r (c + 1)
    else check_all acc r (c + 1)
  in
  check_all 0 0 0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
