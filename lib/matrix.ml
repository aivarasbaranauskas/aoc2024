let map f m = Array.map (Array.map f) m
let mapxy f m = Array.mapi (fun x row -> Array.mapi (fun y v -> f x y v) row) m
let iter f m = Array.iter (Array.iter f) m

let iterxy f m =
  Array.iteri (fun x row -> Array.iteri (fun y v -> f x y v) row) m
