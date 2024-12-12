let file = "inputs/day12/input.txt"

let read_input () =
  List.flatten
  @@ List.mapi (fun r row -> List.mapi (fun c v -> (v, (r, c))) row)
  @@ Shared.F.read_char_matrix_2d_list file

module PairMap = Map.Make (Shared.Sets.IntPair)
module CharMap = Map.Make (Char)

let calculate_group_score_1 (_, m_orig) =
  let m = ref m_orig in
  let rec calculate_area_and_perimeter pos =
    match PairMap.find_opt pos !m with
    | None -> (0, 1)
    | Some visited ->
        if visited then (0, 0)
        else (
          m := PairMap.add pos true !m;
          List.fold_left Shared.Sets.IntPair.add (1, 0)
            [
              calculate_area_and_perimeter @@ Shared.Sets.IntPair.add pos (0, 1);
              calculate_area_and_perimeter @@ Shared.Sets.IntPair.add pos (1, 0);
              calculate_area_and_perimeter @@ Shared.Sets.IntPair.add pos (0, -1);
              calculate_area_and_perimeter @@ Shared.Sets.IntPair.add pos (-1, 0);
            ])
  in
  let calculate_price pos =
    let area, perimeter = calculate_area_and_perimeter pos in
    area * perimeter
  in
  Shared.L.sum
  @@ List.map (fun (pos, _) -> calculate_price pos)
  @@ PairMap.to_list m_orig

(* we can count corners instead of sides *)
let calculate_group_score_2 (_, m_orig) =
  let m = ref m_orig in
  let rec calculate_area_and_corners pos =
    match PairMap.find_opt pos !m with
    | None -> (0, 0)
    | Some visited ->
        if visited then (0, 0)
        else (
          m := PairMap.add pos true !m;
          let is_populated p = PairMap.find_opt p !m != None in
          let is_corner (dc, dr) =
            let dp = is_populated @@ Shared.Sets.IntPair.add pos (dc, dr) in
            let a1 = is_populated @@ Shared.Sets.IntPair.add pos (dc, 0) in
            let a2 = is_populated @@ Shared.Sets.IntPair.add pos (0, dr) in
            if ((not dp) && a1 = a2) || (dp && (not a1) && not a2) then 1 else 0
          in
          let corners =
            is_corner (1, 1)
            + is_corner (-1, -1)
            + is_corner (1, -1)
            + is_corner (-1, 1)
          in
          List.fold_left Shared.Sets.IntPair.add (1, corners)
            [
              calculate_area_and_corners @@ Shared.Sets.IntPair.add pos (0, 1);
              calculate_area_and_corners @@ Shared.Sets.IntPair.add pos (1, 0);
              calculate_area_and_corners @@ Shared.Sets.IntPair.add pos (0, -1);
              calculate_area_and_corners @@ Shared.Sets.IntPair.add pos (-1, 0);
            ])
  in
  let calculate_price pos =
    let area, corners = calculate_area_and_corners pos in
    area * corners
  in
  Shared.L.sum
  @@ List.map (fun (pos, _) -> calculate_price pos)
  @@ PairMap.to_list m_orig

let calculate_price f_price =
  let m = read_input () in
  let grouped =
    List.fold_left
      (fun acc (v, pos) ->
        CharMap.update v
          (function
            | None -> Some PairMap.(empty |> add pos false)
            | Some m -> Some (PairMap.add pos false m))
          acc)
      CharMap.empty m
  in
  Shared.L.sum @@ List.map f_price @@ CharMap.to_list grouped

let part1 () = calculate_price calculate_group_score_1
let part2 () = calculate_price calculate_group_score_2
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
