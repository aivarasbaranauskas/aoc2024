let file = "inputs/day2/input.txt"

let is_safe levels = 
  let rec is_rising levels =
    match levels with
      | a::b::t -> a<b && b-a<=3 && is_rising (b::t)
      | _ -> true in
  let is_falling levels = is_rising @@ List.rev levels in
  is_rising levels || is_falling levels

let is_safe_2 levels =
  let rec is_rising levels d f =
    match levels with
      | a::b::t -> (a<b && b-a<=3 && is_rising (b::t) d false) || (not d && (is_rising (a::t) true false || (f && is_rising (b::t) true false)))
      | _ -> true in
  let is_falling levels = is_rising @@ List.rev levels in
  is_rising levels false true || is_falling levels false true

let () =
  let lines = Shared.File.read_lines file in
    let num_lines = List.map (fun line -> List.map int_of_string @@ String.split_on_char ' ' line) lines in
      let safe_unsafe = List.map is_safe num_lines in
        let result_pt1 = List.fold_left (fun acc a -> if a then acc+1 else acc) 0 safe_unsafe in
      let safe_unsafe_2 = List.map is_safe_2 num_lines in
        let result_pt2 = List.fold_left (fun acc a -> if a then acc+1 else acc) 0 safe_unsafe_2 in
  
  Printf.printf "Part 1: %d\nPart 2: %d\n" result_pt1 result_pt2;