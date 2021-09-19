open! Basis.Rudiments
open! Basis
open U63
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_not %a -> %a\n"
          pp_x x pp_x (bit_not x);
        test xs'
      end
  in
  let xs = [
    0;
    0x7fff_ffff_ffff_ffff
  ] in
  test xs;
  printf "@]"

let _ = test ()
