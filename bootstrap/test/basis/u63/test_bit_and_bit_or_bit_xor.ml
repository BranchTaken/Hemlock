open! Basis.Rudiments
open! Basis
open U63
open Format

let test () =
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "bit_{and,or,xor} %a %a -> %a, %a, %a\n"
          pp_x x pp_x y
          pp_x (bit_and x y)
          pp_x (bit_or x y)
          pp_x (bit_xor x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (0, 0);
    (0x7fff_ffff_ffff_ffff, 0);
    (0, 0x7fff_ffff_ffff_ffff);
    (0x7fff_ffff_ffff_ffff, 0x7fff_ffff_ffff_ffff);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
