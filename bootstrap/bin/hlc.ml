open Hemlock
include Hemlock.Rudiments

let _ =
  let open Format in
  let x = Array.of_list [1; 2; 3] in
  printf "@[<h>";
  printf "Array.length %a -> %a\n"
    (Array.pp Usize.pp) x
    Usize.pp (Array.length x);
  printf "@]"
