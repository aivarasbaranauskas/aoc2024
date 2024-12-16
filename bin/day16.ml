let file = "inputs/day16/input.txt"
let read_input () = Shared.F.read_char_matrix_2d_array file

let find_start map =
  let start_r, start_c = (ref (-1), ref (-1)) in
  Array.iteri
    (fun r line ->
      Array.iteri
        (fun c p ->
          if p = 'S' then (
            start_r := r;
            start_c := c))
        line)
    map;
  map.(!start_r).(!start_c) <- '.';
  (map, (!start_r, !start_c))

let turn_90_degrees d =
  match d with
  | 0, 1 -> (1, 0)
  | 1, 0 -> (0, -1)
  | 0, -1 -> (-1, 0)
  | -1, 0 -> (0, 1)
  | _ -> raise (Failure "bad direction")

let turn_neg_90_degrees d =
  match d with
  | 1, 0 -> (0, 1)
  | 0, -1 -> (1, 0)
  | -1, 0 -> (0, -1)
  | 0, 1 -> (-1, 0)
  | _ -> raise (Failure "bad direction")

let get_from_map m (pr, pc) = m.(pr).(pc)

module PairPairsMap = Map.Make (Shared.Sets.IntPairPair)

let part_1_and_2 () =
  let map = read_input () in
  let map, start = find_start map in
  let mem = ref PairPairsMap.empty in
  let rec walk p d acc =
    let old_n =
      match PairPairsMap.find_opt (p, d) !mem with None -> 0 | Some n -> n
    in
    if old_n != 0 && old_n < acc then None
    else
      match get_from_map map p with
      | '#' -> None
      | 'E' -> Some (acc, Shared.Sets.PairsSet.(empty |> add p))
      | '.' ->
          mem := PairPairsMap.add (p, d) acc !mem;
          let right_d = turn_90_degrees d in
          let left_d = turn_neg_90_degrees d in
          let opts =
            List.filter_map
              (fun x -> x)
              [
                walk (Shared.Sets.IntPair.add p d) d (acc + 1);
                walk (Shared.Sets.IntPair.add p right_d) right_d (acc + 1001);
                walk (Shared.Sets.IntPair.add p left_d) left_d (acc + 1001);
              ]
          in
          if List.length opts = 0 then None
          else
            Some
              (let best_score =
                 Shared.L.min @@ List.map (fun (s, _) -> s) opts
               in
               let best_tiles =
                 List.fold_left Shared.Sets.PairsSet.union
                   Shared.Sets.PairsSet.(empty |> add p)
                 @@ List.filter_map
                      (fun (s, t) -> if s = best_score then Some t else None)
                      opts
               in
               (best_score, best_tiles))
      | c -> raise (Failure (Printf.sprintf "unknown char '%c'" c))
  in
  match walk start (0, 1) 0 with
  | Some (score, tiles) -> (score, Shared.Sets.PairsSet.cardinal tiles)
  | None -> raise (Failure "no path?")

let () =
  let part1, part2 = part_1_and_2 () in
  Printf.printf "Part 1: %d\nPart 2: %d\n" part1 part2
