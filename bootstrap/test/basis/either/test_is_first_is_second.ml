open! Basis.Rudiments
open! Basis
open Either
open Format

let test () =
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "is_first %a -> %b@\n"
          (pp Uns.pp Uns.pp) either (is_first either);
        printf "is_second %a -> %b@\n"
          (pp Uns.pp Uns.pp) either (is_second either);
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
