let file = "inputs/day24/input.txt"

module StringMap = Map.Make (String)

let read_input () =
  let spl = Str.split (Str.regexp_string "\n\n") @@ Shared.F.read_all file in
  let ic1 = Scanf.Scanning.from_string (String.cat (List.hd spl) "\n") in
  let rec read_bits acc =
    match
      Scanf.bscanf_opt ic1 "%s@: %d\n" (fun k v -> StringMap.add k v acc)
    with
    | Some acc -> read_bits acc
    | None -> acc
  in
  let ic2 = Scanf.Scanning.from_string (String.cat (List.nth spl 1) "\n") in
  let rec read_gates acc =
    match
      Scanf.bscanf_opt ic2 "%s %s %s -> %s\n" (fun a ins b dest ->
          StringMap.add dest (a, ins, b) acc)
    with
    | Some acc -> read_gates acc
    | None -> acc
  in
  let bits = read_bits StringMap.empty in
  let gates = read_gates StringMap.empty in
  (bits, gates)

let run wires gates =
  let wires = ref wires in
  let rec execute_gate a inc b =
    let get_val gate =
      match StringMap.find_opt gate !wires with
      | Some v -> v
      | None ->
          let ai, inci, bi = StringMap.find gate gates in
          let v = execute_gate ai inci bi in
          wires := StringMap.add gate v !wires;
          v
    in
    let a_val = get_val a in
    let b_val = get_val b in
    match inc with
    | "AND" -> a_val land b_val
    | "OR" -> a_val lor b_val
    | "XOR" -> a_val lxor b_val
    | x -> raise (Failure (Printf.sprintf "unknwon instruction %s" x))
  in

  StringMap.fold
    (fun dest (a, inc, b) acc ->
      if dest.[0] != 'z' then acc
      else
        let v = execute_gate a inc b in
        let sh = int_of_string (String.sub dest 1 2) in
        acc lor (v lsl sh))
    gates 0

let part1 () =
  let wires, gates = read_input () in
  run wires gates

let int_size = Sys.word_size - 1

let int2bin =
  let buf = Bytes.create int_size in
  fun n ->
    for i = 0 to int_size - 1 do
      let pos = int_size - 1 - i in
      Bytes.set buf pos (if n land (1 lsl i) != 0 then '1' else '0')
    done;
    (* skip leading zeros *)
    match Bytes.index_opt buf '1' with
    | None -> "0b0"
    | Some i -> "0b" ^ Bytes.sub_string buf i (int_size - i)

let part2 () =
  let wires, gates = read_input () in
  let rec analyze zWire =
    match StringMap.find_opt zWire gates with
    | None -> ()
    | Some(a, ins, b) ->
      let n = int_of_string @@ String.sub zWire 1 2 in
      if ins = "XOR" then (
      ) else (
        Printf.printf "BAD %s %s %s -> %s\n" a ins b zWire;
      );
      analyze @@ Printf.sprintf "z%02d" (n+1)
  in
  analyze "z03";
  let wires_x_1_y_0 =
    StringMap.mapi (fun k _ -> if k.[0] = 'x' then 1 else 0) wires
  in
  let wires_x_0_y_1 =
    StringMap.mapi (fun k _ -> if k.[0] = 'y' then 1 else 0) wires
  in
  let wires_x_1_y_1 = StringMap.map (fun _ -> 1) wires in

  Printf.printf "%s\n" @@ int2bin @@ run wires_x_1_y_0 gates;
  Printf.printf "%s\n" @@ int2bin @@ run wires_x_0_y_1 gates;
  Printf.printf "%s\n" @@ int2bin @@ run wires_x_1_y_1 gates;
  0

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())