let file = "inputs/day22/input.txt"
let read_input () = List.map int_of_string @@ Shared.F.read_lines file

let rec gen_secret_numbers x () =
  let x2 = x in
  let x2 = x2 * 64 lxor x2 mod 16777216 in
  let x2 = x2 / 32 lxor x2 mod 16777216 in
  let x2 = x2 * 2048 lxor x2 mod 16777216 in
  Seq.Cons (x, gen_secret_numbers x2)

let gen_nth_secret_number n x =
  let seq = gen_secret_numbers x in
  let seq = Seq.drop n seq in
  List.hd @@ List.of_seq @@ Seq.take 1 seq

let gen_prices x = Seq.map (fun num -> num mod 10) @@ gen_secret_numbers x

let part1 () =
  let numbers = read_input () in
  Shared.L.sum @@ List.map (gen_nth_secret_number 2000) numbers

module QuadsMap = Map.Make (Shared.Sets.IntQuad)

let part2 () =
  let calc_number_changes_map number =
    let rec group_price_changes acc = function
      | p1 :: p2 :: p3 :: p4 :: p5 :: tl ->
          let changes = (p2 - p1, p3 - p2, p4 - p3, p5 - p4) in
          group_price_changes ((changes, p5) :: acc) (p2 :: p3 :: p4 :: p5 :: tl)
      | _ -> List.rev acc
    in

    let prices_seq = List.of_seq @@ Seq.take 2001 @@ gen_prices number in
    let grouped = group_price_changes [] prices_seq in
    let deduplicated =
      List.fold_left
        (fun acc (k, v) ->
          if QuadsMap.find_opt k acc = None then QuadsMap.add k v acc else acc)
        QuadsMap.empty grouped
    in
    deduplicated
  in

  let numbers = read_input () in
  let maps = List.map calc_number_changes_map numbers in
  let collapsed =
    List.fold_left
      (QuadsMap.union (fun _ a b -> Some (a + b)))
      QuadsMap.empty maps
  in
  let best = QuadsMap.fold (fun _ v acc -> max v acc) collapsed 0 in
  best

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
