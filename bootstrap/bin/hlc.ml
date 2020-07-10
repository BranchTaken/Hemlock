open Basis
include Basis.Rudiments

let _ =
  let open Format in
  let x = Array.of_list [1; 2; 3] in
  printf "@[<h>";
  printf "Array.length %a -> %a\n"
    (Array.pp Uns.pp) x
    Uns.pp (Array.length x);
  printf "@]"
