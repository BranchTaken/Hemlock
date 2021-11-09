open! Basis.Rudiments
open! Basis
open U256
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
    zero;
    max_value;
  ] in
  test xs;
  printf "@]"

let _ = test ()
