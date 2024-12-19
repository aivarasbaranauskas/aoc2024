let file = "inputs/day19/input.txt"

let read_input () =
  let lines = Shared.F.read_lines file in
  let spl = List.map String.trim @@ String.split_on_char ',' @@ List.hd lines in
  (spl, List.tl @@ List.tl lines)

module StringMap = Map.Make (String)

let part1 () =
  let towels, combos = read_input () in
  let mem = ref StringMap.empty in
  let rec check_combo combo =
    match StringMap.find_opt combo !mem with
    | Some v -> v
    | None ->
        let rec check_towels = function
          | [] -> false
          | t :: ts ->
              if String.starts_with ~prefix:t combo then
                if t = combo then true
                else
                  let tl = String.length t in
                  let cl = String.length combo in
                  if check_combo (String.sub combo tl (cl - tl)) then true
                  else check_towels ts
              else check_towels ts
        in
        let v = check_towels towels in
        mem := StringMap.add combo v !mem;
        v
  in
  List.fold_left (fun acc c -> acc + if check_combo c then 1 else 0) 0 combos

let part2 () =
  let towels, combos = read_input () in
  let mem = ref StringMap.empty in
  let rec check_combo combo =
    match StringMap.find_opt combo !mem with
    | Some v -> v
    | None ->
        let rec check_towels acc = function
          | [] -> acc
          | t :: ts ->
              if String.starts_with ~prefix:t combo then
                let tl = String.length t in
                let cl = String.length combo in
                let v =
                  if t = combo then 1
                  else check_combo (String.sub combo tl (cl - tl))
                in
                check_towels (acc + v) ts
              else check_towels acc ts
        in
        let v = check_towels 0 towels in
        mem := StringMap.add combo v !mem;
        v
  in
  List.fold_left (fun acc c -> acc + check_combo c) 0 combos

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
