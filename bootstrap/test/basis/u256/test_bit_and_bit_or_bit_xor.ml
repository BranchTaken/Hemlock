open! Basis.Rudiments
open! Basis
open U256
open Format

let test () =
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "bit_{and,or,xor} %a %a -> %a, %a, %a\n"
          xpp_x x xpp_x y
          xpp_x (bit_and x y)
          xpp_x (bit_or x y)
          xpp_x (bit_xor x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (zero, zero);
    (max_value, zero);
    (zero, max_value);
    (max_value, max_value);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
