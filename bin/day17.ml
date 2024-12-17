let file = "inputs/day17/input.txt"

let read_input () =
  let ic = Scanf.Scanning.from_file file in
  let reg_a = Scanf.bscanf ic "Register A: %d\n" (fun x -> x) in
  let reg_b = Scanf.bscanf ic "Register B: %d\n" (fun x -> x) in
  let reg_c = Scanf.bscanf ic "Register C: %d\n" (fun x -> x) in
  let program =
    Scanf.bscanf ic "\nProgram: %s" (fun s ->
        Array.of_list @@ List.map int_of_string @@ String.split_on_char ',' s)
  in
  (reg_a, reg_b, reg_c, program)

let run_program reg_a_init reg_b_init reg_c_init program =
  let out, reg_a, reg_b, reg_c =
    (ref [], ref reg_a_init, ref reg_b_init, ref reg_c_init)
  in
  let rec run i =
    if i >= Array.length program then ()
    else
      let opcode, operand = (program.(i), program.(i + 1)) in
      let combo_operand =
        match operand with
        | 0 -> 0
        | 1 -> 1
        | 2 -> 2
        | 3 -> 3
        | 4 -> !reg_a
        | 5 -> !reg_b
        | 6 -> !reg_c
        | 7 -> -1
        | x -> raise (Failure (Printf.sprintf "unknown combo operand %d" x))
      in

      match opcode with
      | 0 ->
          reg_a := !reg_a / Shared.M.pow 2 combo_operand;
          run (i + 2)
      | 1 ->
          reg_b := !reg_b lxor operand;
          run (i + 2)
      | 2 ->
          reg_b := Shared.M.modulo combo_operand 8;
          run (i + 2)
      | 3 -> if !reg_a = 0 then run (i + 2) else run operand
      | 4 ->
          reg_b := !reg_b lxor !reg_c;
          run (i + 2)
      | 5 ->
          out := Shared.M.modulo combo_operand 8 :: !out;
          run (i + 2)
      | 6 ->
          reg_b := !reg_a / Shared.M.pow 2 combo_operand;
          run (i + 2)
      | 7 ->
          reg_c := !reg_a / Shared.M.pow 2 combo_operand;
          run (i + 2)
      | x -> raise (Failure (Printf.sprintf "unknown opcode %d" x))
  in
  run 0;
  List.rev !out

let part1 () =
  let reg_a_init, reg_b_init, reg_c_init, program = read_input () in
  let out = run_program reg_a_init reg_b_init reg_c_init program in
  String.concat "," @@ List.map string_of_int out

let part2 () =
  (* only works with real input, not test case.. *)
  let _, reg_b_init, reg_c_init, program = read_input () in
  (* assume last operation is a loop, so remove it *)
  let shorter_program = Array.sub program 0 (Array.length program) in
  let run_prog_for_next_didigt a =
    let out = run_program a reg_b_init reg_c_init shorter_program in
    List.hd out
  in
  (* reverse calc *)
  let rec construct acc = function
    | [] -> Some acc
    | h :: t ->
        let acc = acc * 8 in
        let i = ref 0 in
        let x = ref None in
        while !x = None && !i < 8 do
          if h = run_prog_for_next_didigt (acc + !i) then x := construct (acc + !i) t;
          i := !i + 1
        done;
        !x
  in
  let out = construct 0 @@ List.rev @@ Array.to_list program in
  match out with Some x -> x | None -> -1

let () = Printf.printf "Part 1: %s\nPart 2: %d\n" (part1 ()) (part2 ())
