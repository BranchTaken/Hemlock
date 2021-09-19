open! Basis.Rudiments
open! Basis
open! Bool
open Format

let test () =
  let side_effect b s = begin
    printf "side effect %s\n" s;
    b
  end in
  let rec fn pairs = begin
    match pairs with
    | [] -> ()
    | (a, b) :: pairs' -> begin
        printf "(%b || %b) -> %b\n"
          a b ((side_effect a "a") || (side_effect b "b"));
        fn pairs'
      end
  end in
  fn [(false, false); (false, true); (true, false); (true, true)]

let _ = test ()
