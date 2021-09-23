open! Basis.Rudiments
open! Basis
open I16
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
    kv 0L;
    kv 0xffffL
  ] in
  test xs;
  printf "@]"

let _ = test ()
