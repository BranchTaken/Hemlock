open! Basis.Rudiments
open! Basis
open U128
open Format

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        printf "%a %a\n" pp x pp_x x;
        fn xs'
      end
  in
  printf "@[<h>";
  fn [
    zero;
    one;
    of_string "42";
    min_value;
    max_value
  ];
  printf "@]"

let _ = test ()
