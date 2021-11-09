open! Basis.Rudiments
open! Basis
open Either
open Format

let test () =
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "value_map %a -> %a@\n"
          (xpp Uns.xpp Uns.xpp) either Uns.xpp
          (value_map ~first:(fun x -> x + 2L) ~second:(fun x -> x + 4L) either);
        fn eithers'
      end
  in
  let eithers = [
    First 1L;
    Second 2L;
  ] in
  fn eithers;
  printf "@]"

let _ = test ()
