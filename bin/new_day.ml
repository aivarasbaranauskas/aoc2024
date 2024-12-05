let usage_msg = "new_day <day>"
let day = ref ""

let anon_fun arg =
  if !day = "" then day := arg else raise (Arg.Bad "too many arguments")

let program_template = format_of_string "let file = \"inputs/day%s/input.txt\"

let read_input () = ()

let part1 () = 0

let part2 () = 0

let () = Printf.printf \"Part 1: %%d\\nPart 2: %%d\\n\" (part1 ()) (part2 ())"

let dune_entry_template = format_of_string "\n(executable
 (public_name day%s)
 (name day%s)
 (libraries shared))\n"


let () = 
  Arg.parse [] anon_fun usage_msg;
  
  let oc = open_out (Printf.sprintf "bin/day%s.ml" !day) in
  Printf.fprintf oc program_template !day;
  close_out oc;

  let oc = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 "bin/dune" in
  Printf.fprintf oc dune_entry_template !day !day;
  close_out oc;