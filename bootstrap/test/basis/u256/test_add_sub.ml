open! Basis.Rudiments
open! Basis
open U256
open Format

let test () =
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a +,- %a -> %a, %a\n" pp_x x pp_x y pp_x (x + y) pp_x (x - y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");

    (of_string "0", of_string "1");
    (of_string "1", of_string "0");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
