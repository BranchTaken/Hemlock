open! Basis.Rudiments
open! Basis
open I256
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_lg,ceil_lg %a -> %a, %a\n"
          pp_x u
          pp (floor_lg u)
          pp (ceil_lg u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";
    max_value;
  ] in
  test us;
  printf "@]"

let _ = test ()
