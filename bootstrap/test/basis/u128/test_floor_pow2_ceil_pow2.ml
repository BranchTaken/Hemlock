open! Basis.Rudiments
open! Basis
open U128
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_pow2,ceil_pow2 %a -> %a, %a\n"
          pp_x u
          pp_x (floor_pow2 u)
          pp_x (ceil_pow2 u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]"

let _ = test ()
