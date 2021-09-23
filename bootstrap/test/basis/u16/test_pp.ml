open! Basis.Rudiments
open! Basis
open U16
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
  fn [kv 0L; kv 1L; kv 42L; kv 0x1fffL];
  printf "@]"

let _ = test ()
