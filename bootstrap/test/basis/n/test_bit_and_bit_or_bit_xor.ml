open! Basis.Rudiments
open! Basis
open N
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
  let u64_max = of_u64 U64.max_value in
  let pairs = [
    (zero, zero);
    (u64_max, zero);
    (zero, u64_max);
    (u64_max, u64_max);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
