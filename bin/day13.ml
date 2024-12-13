let file = "inputs/day13/input.txt"

let input =
  let r =
    Str.regexp
      {|Button A: X\+\([0-9]+\), Y\+\([0-9]+\)
Button B: X\+\([0-9]+\), Y\+\([0-9]+\)
Prize: X=\([0-9]+\), Y=\([0-9]+\)|}
  in
  List.map (fun s ->
      let _ = Str.search_forward r s 0 in
      ( ( int_of_string (Str.matched_group 1 s),
          int_of_string (Str.matched_group 2 s) ),
        ( int_of_string (Str.matched_group 3 s),
          int_of_string (Str.matched_group 4 s) ),
        ( int_of_string (Str.matched_group 5 s),
          int_of_string (Str.matched_group 6 s) ) ))
  @@ Str.split (Str.regexp_string "\n\n")
  @@ Shared.F.read_all file

let find_cheapest ((ax, ay), (bx, by), (x, y)) =
  let top = ax*y - ay*x in
  let bot = ax*by - bx*ay in
  let price = if top mod bot != 0 then 0 else 
    (
      let b = top / bot in
      let top2 = x - bx*b in
      if top2 mod ax != 0 then 0 else
      (let a = top2/ax in
      a*3+b)
    )
  in
  price

let part1 () = Shared.L.sum @@ List.map find_cheapest input
let part2 () = 
  let modified_input = List.map (fun (a, b, (x, y)) -> (a, b, (x+10000000000000, y+10000000000000))) input in
  Shared.L.sum @@ List.map find_cheapest modified_input

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 ()) (part2 ())
