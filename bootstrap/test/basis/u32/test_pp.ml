open! Basis.Rudiments
open! Basis
open U32
open Format

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        printf "%a %a\n" xpp x xpp_x x;
        fn xs'
      end
  in
  printf "@[<h>";
  fn [kv 0L; kv 1L; kv 42L; kv 0x1_ffff_ffffL];
  printf "@]"

let _ = test ()
