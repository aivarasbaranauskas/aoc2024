let explode_to_list s = List.init (String.length s) (String.get s)
let explode_to_array s = Array.init (String.length s) (String.get s)

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
