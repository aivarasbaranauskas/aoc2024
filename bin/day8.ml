let file = "inputs/day8/input.txt"

let collect_antenas m =
  let antenas =
    List.mapi
      (fun ri row ->
        List.mapi
          (fun ci cell ->
            match cell with '.' -> None | cell -> Some (ri, ci, cell))
          row)
      m
  in
  List.filter_map (fun x -> x) @@ List.flatten antenas

let group_antenas acc (ri, ci, freq) =
  match acc with
  | [] -> [ (freq, [ (ri, ci) ]) ]
  | (head_freq, coords) :: t ->
      if head_freq == freq then (head_freq, (ri, ci) :: coords) :: t
      else (freq, [ (ri, ci) ]) :: (head_freq, coords) :: t

let read_input () =
  let m = Shared.F.read_char_matrix_2d_list file in
  let map_size = (List.length m, List.length @@ List.hd m) in
  let antenas = collect_antenas m in
  let sorted_antenas =
    List.sort (fun (_, _, freq1) (_, _, freq2) -> compare freq1 freq2) antenas
  in
  let grouped_antenas = List.fold_left group_antenas [] sorted_antenas in
  (grouped_antenas, map_size)

module IntPair = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c

  let add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)
  let rem (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)
  let mul (x0, y0) (x1, y1) = (x0 * x1, y0 * y1)
end

module PairsSet = Set.Make (IntPair)

let solve resolver =
  let grouped_antenas, map_size = read_input () in
  let grouped_antinodes = List.map (resolver map_size) grouped_antenas in
  let antinodes =
    List.fold_left PairsSet.union PairsSet.empty grouped_antinodes
  in
  List.length @@ PairsSet.to_list antinodes

let generate_antinodes map_height map_width coord1 coord2 =
  let the_diff = IntPair.rem coord1 coord2 in
  PairsSet.(
    empty
    |> add (IntPair.rem coord2 the_diff)
    |> add (IntPair.add coord1 the_diff)
    |> filter (fun (r, c) ->
           r >= 0 && c >= 0 && r < map_height && c < map_width))

let generate_antinodes_2 map_height map_width coord1 coord2 =
  let the_diff = IntPair.rem coord1 coord2 in
  let rec do_gen acc (r, c) offset =
    if r >= 0 && c >= 0 && r < map_height && c < map_width then
      do_gen (PairsSet.add (r, c) acc) (IntPair.add (r, c) offset) offset
    else acc
  in
  PairsSet.(
    union
      (do_gen PairsSet.empty coord1 the_diff)
      (do_gen PairsSet.empty coord2 (IntPair.mul the_diff (-1, -1))))

let resolve_antinodes generate_antinodes (map_height, map_width) (_, all_coords)
    =
  let rec collect2 antinodes coord1 coords =
    match coords with
    | [] -> antinodes
    | coord2 :: t ->
        let antinodes =
          PairsSet.union antinodes
            (generate_antinodes map_height map_width coord1 coord2)
        in
        collect2 antinodes coord1 t
  in
  let rec collect1 antinodes coords =
    match coords with
    | [] -> antinodes
    | coord :: t -> collect1 (collect2 antinodes coord t) t
  in
  collect1 PairsSet.empty all_coords

let part1 () = solve (resolve_antinodes generate_antinodes)
let part2 () = solve (resolve_antinodes generate_antinodes_2)
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
