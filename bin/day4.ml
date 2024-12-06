let file = "inputs/day4/input.txt"
let read_input () = Shared.F.read_char_matrix_2d_array file

let count_from_direction m row col row_d col_d =
  let max_row = row + (3 * row_d) in
  let max_col = col + (3 * col_d) in
  if
    max_row < 0
    || max_row >= Array.length m
    || max_col < 0
    || max_col >= Array.length m.(0)
  then 0
  else if
    m.(row + row_d).(col + col_d) = 'M'
    && m.(row + (2 * row_d)).(col + (2 * col_d)) = 'A'
    && m.(row + (3 * row_d)).(col + (3 * col_d)) = 'S'
  then 1
  else 0

let count_from m row col =
  count_from_direction m row col 0 1
  + count_from_direction m row col 1 0
  + count_from_direction m row col 0 (-1)
  + count_from_direction m row col (-1) 0
  + count_from_direction m row col 1 1
  + count_from_direction m row col (-1) (-1)
  + count_from_direction m row col 1 (-1)
  + count_from_direction m row col (-1) 1

let part1 () =
  let ct = ref 0 in
  let m = read_input () in
  Array.iteri
    (fun row line ->
      Array.iteri
        (fun col char -> if char = 'X' then ct := !ct + count_from m row col)
        line)
    m;
  !ct

let part2 () =
  let ct = ref 0 in
  let m = read_input () in
  let height = Array.length m in
  let width = Array.length m.(0) in
  for row = 1 to height - 2 do
    for col = 1 to width - 2 do
      if
        m.(row).(col) = 'A'
        && ((m.(row - 1).(col - 1) = 'M' && m.(row + 1).(col + 1) = 'S')
           || (m.(row - 1).(col - 1) = 'S' && m.(row + 1).(col + 1) = 'M'))
        && ((m.(row - 1).(col + 1) = 'M' && m.(row + 1).(col - 1) = 'S')
           || (m.(row - 1).(col + 1) = 'S' && m.(row + 1).(col - 1) = 'M'))
      then ct := !ct + 1
    done
  done;
  !ct

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
