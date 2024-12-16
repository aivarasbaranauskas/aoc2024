let sum l = List.fold_left (fun acc a -> acc + a) 0 l
let sort l = List.sort Stdlib.compare l
let min l = List.fold_left min max_int l
