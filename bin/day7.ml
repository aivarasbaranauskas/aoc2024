let file = "inputs/day7/input.txt"

let read_input () =
  let lines = Shared.F.read_lines file in
  List.map
    (fun line ->
      let spl = String.split_on_char ':' line in
      let test_value = int_of_string @@ List.hd spl in
      let numbers =
        List.map int_of_string @@ String.split_on_char ' ' @@ String.trim
        @@ List.nth spl 1
      in
      (test_value, numbers))
    lines

let can_calibration_equation_be_true (test_value, numbers) =
  let rec check result left_numbers =
    match left_numbers with
    | [] -> result = test_value
    | h :: t -> check (result + h) t || check (result * h) t
  in
  match numbers with h :: t -> check h t | [] -> raise (Failure "wth")

let part1 () =
  let calibration_equations = read_input () in
  let good_calibration_equations =
    List.filter can_calibration_equation_be_true calibration_equations
  in
  let good_test_values =
    List.map (fun (test_value, _) -> test_value) good_calibration_equations
  in
  Shared.L.sum good_test_values

let join_ints a b = int_of_string (string_of_int a ^ string_of_int b)

let can_calibration_equation_be_true_2 (test_value, numbers) =
  let rec check result left_numbers =
    match left_numbers with
    | [] -> result = test_value
    | h :: t ->
        check (result + h) t
        || check (result * h) t
        || check (join_ints result h) t
  in
  match numbers with h :: t -> check h t | [] -> raise (Failure "wth")

let part2 () =
  let calibration_equations = read_input () in
  let good_calibration_equations =
    List.filter can_calibration_equation_be_true_2 calibration_equations
  in
  let good_test_values =
    List.map (fun (test_value, _) -> test_value) good_calibration_equations
  in
  Shared.L.sum good_test_values

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
