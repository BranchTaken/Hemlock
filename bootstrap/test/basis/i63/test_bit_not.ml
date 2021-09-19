open! Basis.Rudiments
open! Basis
open I63
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
    kv 0;
    kv 0x7fff_ffff_ffff_ffff
  ] in
  test xs;
  printf "@]"

let _ = test ()
