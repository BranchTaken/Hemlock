open! Basis.Rudiments
open! Basis
open Either
open Format

let test () =
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "value %a -> %a@\n"
          (xpp Uns.xpp Uns.xpp) either Uns.xpp (value either);
        fn eithers'
      end
  in
  let eithers = [
    First 0L;
    Second 0L;
  ] in
  fn eithers;
  printf "@]"

let _ = test ()
