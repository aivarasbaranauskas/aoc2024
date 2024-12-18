let file = "inputs/day18/input.txt"

let read_input () =
  List.map (fun line ->
      let spl = String.split_on_char ',' line in
      (int_of_string @@ List.hd spl, int_of_string @@ List.hd @@ List.tl spl))
  @@ Shared.F.read_lines file

(* let grid_size = 6 *)
let grid_size = 70

let get_grid n bytes =
  let grid = Array.make_matrix (grid_size + 1) (grid_size + 1) '.' in
  let rec fill_grid bytes = function
    | 0 -> ()
    | i ->
        let x, y = List.hd bytes in
        grid.(x).(y) <- '#';
        fill_grid (List.tl bytes) (i - 1)
  in
  fill_grid bytes n;
  grid

let reconstruct_path cameFrom current =
  let rec reconstruct acc current =
    match Shared.Sets.PairsMap.find_opt current cameFrom with
    | None -> List.rev (current :: acc)
    | Some x -> reconstruct (current :: acc) x
  in
  reconstruct [] current

let a_star start goal h =
  let openSet = ref Shared.Sets.PairsSet.(empty |> add start) in
  let cameFrom = ref Shared.Sets.PairsMap.empty in
  let gScore = ref Shared.Sets.PairsMap.(empty |> add start 0) in
  let fScore = ref Shared.Sets.PairsMap.(empty |> add start (h start)) in

  let get_from_pairs_map key map =
    match Shared.Sets.PairsMap.find_opt key map with
    | Some value -> value
    | None -> Int.max_int
  in

  let rec iterate () =
    if Shared.Sets.PairsSet.is_empty !openSet then None
    else
      let current =
        List.fold_left
          (fun acc a ->
            match acc with
            | None -> Some a
            | Some b ->
                if get_from_pairs_map a !fScore < get_from_pairs_map b !fScore
                then Some a
                else Some b)
          None
        @@ Shared.Sets.PairsSet.to_list !openSet
      in
      let current =
        match current with None -> raise (Failure "uh oh") | Some x -> x
      in
      if current = goal then Some (reconstruct_path !cameFrom current)
      else (
        openSet := Shared.Sets.PairsSet.remove current !openSet;
        let directions = [ (1, 0); (0, 1); (-1, 0); (0, -1) ] in
        List.iter
          (fun direction ->
            let neighbor = Shared.Sets.IntPair.add current direction in
            let hh = h neighbor in
            let tentative_gScore = get_from_pairs_map current !gScore + 1 in
            if
              hh < Int.max_int
              && tentative_gScore < get_from_pairs_map neighbor !gScore
            then (
              cameFrom := Shared.Sets.PairsMap.add neighbor current !cameFrom;
              gScore :=
                Shared.Sets.PairsMap.add neighbor tentative_gScore !gScore;
              fScore :=
                Shared.Sets.PairsMap.add neighbor (tentative_gScore + hh)
                  !fScore;
              openSet := Shared.Sets.PairsSet.add neighbor !openSet))
          directions;
        iterate ())
  in
  iterate ()

let heuristic grid (px, py) =
  if px < 0 || py < 0 || px > grid_size || py > grid_size then Int.max_int
  else
    match grid.(px).(py) with
    | '#' -> Int.max_int
    | '.' -> (2 * grid_size) - px - py
    | c -> raise (Failure (Printf.sprintf "bad grid value %c" c))

let part1 () =
  let bytes = read_input () in
  (* let n = 12 in *)
  let n = 1024 in
  let grid = get_grid n bytes in
  let path = a_star (0, 0) (grid_size, grid_size) (heuristic grid) in
  match path with Some path -> List.length path - 1 | None -> -1

let part2 () =
  let bytes = read_input () in
  (* let n = 12 in *)
  let n = 1024 in
  let grid = get_grid n bytes in
  let rec find_no_path n =
    let x, y = List.nth bytes n in
    grid.(x).(y) <- '#';
    let path = a_star (0, 0) (grid_size, grid_size) (heuristic grid) in
    match path with None -> n | Some _ -> find_no_path (n + 1)
  in
  let n = find_no_path (n + 1) in
  Shared.Sets.IntPair.to_string @@ List.nth bytes n

let () = Printf.printf "Part 1: %d\nPart 2: %s\n" (part1 ()) (part2 ())
