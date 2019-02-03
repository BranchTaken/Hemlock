open SawML
include SawML.Rudiments

type 'a v = 'a array

let _ =
  let x:(int v) = Array.of_list [1; 2; 3] in
  Printf.printf "Array.length x -> %d\n" (Array.length x);
  1
