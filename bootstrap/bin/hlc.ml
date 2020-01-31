open Hemlock
include Hemlock.Rudiments

type 'a v = 'a array

let _ =
  let x:(int v) = Array.of_list [1; 2; 3] in
  Printf.printf "Array.length x -> %d\n" (Usize.to_int (Array.length x));
  1
