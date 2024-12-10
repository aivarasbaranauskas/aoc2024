let file = "inputs/day9/input.txt"

let read_input () =
  List.map (fun x -> int_of_char x - int_of_char '0')
  @@ Shared.F.read_char_list file

let expand disk_map =
  let l = Shared.L.sum disk_map in
  let arr = Array.make l (-1) in
  let do_fill = Array.fill arr in
  let rec do_expand i i2 dm =
    match dm with
    | [] -> ()
    | [ x ] -> do_fill i x i2
    | x1 :: x2 :: t ->
        do_fill i x1 i2;
        do_expand (i + x1 + x2) (i2 + 1) t
  in
  do_expand 0 0 disk_map;
  arr

let calculate_hash disk_map =
  Shared.L.sum @@ Array.to_list
  @@ Array.mapi (fun i id -> if id = -1 then 0 else id * i) disk_map

let part1 () =
  let disk_map = expand @@ read_input () in
  let rec rearrange i1 i2 =
    if i1 >= i2 then ()
    else if disk_map.(i1) != -1 then rearrange (i1 + 1) i2
    else if disk_map.(i2) = -1 then rearrange i1 (i2 - 1)
    else (
      disk_map.(i1) <- disk_map.(i2);
      disk_map.(i2) <- -1;
      rearrange (i1 + 1) (i2 - 1))
  in
  rearrange 0 (Array.length disk_map - 1);
  calculate_hash disk_map

let rec split acc l id =
  match l with
  | [] -> Array.of_list @@ List.rev acc
  | [ x ] -> split ((x, 0, id) :: acc) [] (id + 1)
  | x1 :: x2 :: t -> split ((x1, x2, id) :: acc) t (id + 1)

let expand_2 disk_map =
  Array.of_list @@ List.flatten
  @@ List.map
       (fun (f, s, id) -> List.init (f + s) (fun i -> if i < f then id else -1))
       (Array.to_list disk_map)

let part2 () =
  let disk_map = read_input () in
  let disk_map = split [] disk_map 0 in
  let rec rearrange i1 =
    let rec rearrange_inner i2 =
      if i2 >= i1 then false
      else
        let f1, s1, id1 = disk_map.(i1) in
        let f2, s2, id2 = disk_map.(i2) in
        if s2 < f1 then rearrange_inner (i2 + 1)
        else (
          Array.blit disk_map (i2 + 1) disk_map (i2 + 2) (i1 - i2 - 1);
          disk_map.(i2) <- (f2, 0, id2);
          disk_map.(i2 + 1) <- (f1, s2 - f1, id1);
          let f3, s3, id3 = disk_map.(i1) in
          disk_map.(i1) <- (f3, s3 + f1 + s1, id3);
          true)
    in
    if i1 < 0 then ()
    else if rearrange_inner 0 then rearrange i1
    else rearrange (i1 - 1)
  in
  rearrange (Array.length disk_map - 1);
  let expanded = expand_2 disk_map in
  calculate_hash expanded

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
