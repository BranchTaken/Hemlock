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
          xpp_x u
          xpp (floor_lg u)
          xpp (ceil_lg u);
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
