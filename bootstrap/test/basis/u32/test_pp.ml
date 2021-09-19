open! Basis.Rudiments
open! Basis
open U32
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
  fn [kv 0; kv 1; kv 42; kv 0x1_ffff_ffff];
  printf "@]"

let _ = test ()
