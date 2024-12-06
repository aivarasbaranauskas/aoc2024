let read_lines name =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let read_lines_to_array name = Array.of_list @@ read_lines name

let read_all name =
  let ch = open_in name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let read_char_matrix_2d_array name =
  let lines = read_lines_to_array name in
  Array.map S.explode_to_array lines
