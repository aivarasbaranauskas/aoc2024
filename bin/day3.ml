let file = "inputs/day3/input.txt"

let collect_multiplications s =
  let r = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let start = ref 0 in
  let try_match_next () =
    try
      Some
        (let _ = Str.search_forward r s !start in
         start := Str.match_end ();
         ( int_of_string (Str.matched_group 1 s),
           int_of_string (Str.matched_group 2 s) ))
    with Not_found -> None
  in

  let rec loop acc =
    match try_match_next () with
    | Some s -> loop (s :: acc)
    | None -> List.rev acc
  in
  loop []

let collect_enabled_multiplications s =
  let r = Str.regexp {|don't()\|do()|} in
  let splits = Str.full_split r s in

  let enabled = ref true in
  List.flatten
  @@ List.map
       (fun s ->
         match s with
         | Str.Delim d ->
             enabled := d = "do()";
             []
         | Str.Text ss -> if !enabled then collect_multiplications ss else [])
       splits

let () =
  let input = Shared.File.read_all file in
  let result_pt1 =
    Shared.L.sum
    @@ List.map (fun (a, b) -> a * b) (collect_multiplications input)
  in
  let result_pt2 =
    Shared.L.sum
    @@ List.map (fun (a, b) -> a * b) (collect_enabled_multiplications input)
  in

  Printf.printf "Part 1: %d\nPart 2: %d\n" result_pt1 result_pt2
