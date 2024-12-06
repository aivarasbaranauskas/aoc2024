let file = "inputs/day5/input.txt"

let read_input () =
  let rec parse lines acc =
    match lines with
    | "" :: tail ->
        ( acc,
          List.map
            (fun line ->
              List.map int_of_string @@ String.split_on_char ',' line)
            tail )
    | s :: tail ->
        let spl = String.split_on_char '|' s in
        parse tail
          ((int_of_string @@ List.nth spl 0, int_of_string @@ List.nth spl 1)
          :: acc)
    | _ -> raise (Failure "bad")
  in
  let lines = Shared.F.read_lines file in
  parse lines []

let check_update rules update =
  let rec check u =
    match u with
    | [] -> true
    | ui :: t ->
        let fails_any_rule =
          List.exists (fun (a, b) -> ui = b && List.mem a t) rules
        in
        if fails_any_rule then false else check t
  in
  check update

let fix_update rules update =
  let rec fix h u =
    match u with
    | [] -> List.rev h
    | ui :: t -> (
        let failing_rule =
          List.find_opt (fun (a, b) -> ui = b && List.mem a t) rules
        in
        match failing_rule with
        | None -> fix (ui :: h) t
        | Some (a, b) ->
            let new_t = List.filter (fun x -> x != a) t in
            fix [] (List.rev_append h (a :: b :: new_t)))
  in
  fix [] update

let sum_middles updates =
  let middles = List.map (fun u -> List.nth u (List.length u / 2)) updates in
  Shared.L.sum middles

let part1 () =
  let rules, updates = read_input () in
  let filtered_good_updates = List.filter (check_update rules) updates in
  sum_middles filtered_good_updates

let part2 () =
  let rules, updates = read_input () in
  let filtered_bad_updates =
    List.filter (fun u -> not @@ check_update rules u) updates
  in
  let fixed_updates = List.map (fix_update rules) filtered_bad_updates in
  sum_middles fixed_updates

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
