let file = "inputs/day23/input.txt"

let read_input () =
  List.map (fun line ->
      let spl = String.split_on_char '-' line in
      (List.hd spl, List.nth spl 1))
  @@ Shared.F.read_lines file

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module StringSetSet = Set.Make (StringSet)

let append_map_list a b map =
  let map =
    StringMap.add a
      (match StringMap.find_opt a map with
      | Some set -> StringSet.add b set
      | None -> StringSet.(empty |> add b))
      map
  in
  let map =
    StringMap.add b
      (match StringMap.find_opt b map with
      | Some set -> StringSet.add a set
      | None -> StringSet.(empty |> add a))
      map
  in
  map

let part1 () =
  let connections = read_input () in
  let map =
    List.fold_left
      (fun acc (a, b) -> append_map_list a b acc)
      StringMap.empty connections
  in
  let triplets = ref StringSetSet.empty in
  StringMap.iter
    (fun p1 vs ->
      StringSet.iter
        (fun p2 ->
          StringSet.iter (fun p3 ->
              if StringSet.mem p1 @@ StringMap.find p3 map then
                let x = StringSet.of_list [ p1; p2; p3 ] in
                triplets := StringSetSet.add x !triplets)
          @@ StringMap.find p2 map)
        vs)
    map;
  let filtered_triplets =
    StringSetSet.filter
      (fun triplet ->
        not
          (StringSet.for_all
             (fun v -> not (String.starts_with ~prefix:"t" v))
             triplet))
      !triplets
  in
  StringSetSet.cardinal filtered_triplets

module G = Graph.Persistent.Graph.Concrete (String)
module BK = Graph.Clique.Bron_Kerbosch (G)

let part2 () =
  let edges = read_input () in
  let vertices =
    List.fold_left
      (fun acc (a, b) -> StringSet.(add a acc |> add b))
      StringSet.empty edges
  in
  let g =
    StringSet.fold (fun a graph -> G.add_vertex graph a) vertices G.empty
  in
  let g = List.fold_left (fun graph (a, b) -> G.add_edge graph a b) g edges in
  let cliques = BK.maximalcliques g in
  let longext_clique =
    List.fold_left
      (fun acc clique ->
        if List.length acc < List.length clique then clique else acc)
      [] cliques
  in
  String.concat "," longext_clique

let () = Printf.printf "Part 1: %d\nPart 2: %s\n" (part1 ()) (part2 ())
