open! Basis.Rudiments
open! Basis
open U16
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
    (kv 0L, kv 0L);
    (kv 0xffffL, kv 0L);
    (kv 0L, kv 0xffffL);
    (kv 0xffffL, kv 0xffffL);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
