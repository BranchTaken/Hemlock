open! Basis.Rudiments
open! Basis
open U64
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_not %a -> %a\n"
          xpp_x x xpp_x (bit_not x);
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]"

let _ = test ()
