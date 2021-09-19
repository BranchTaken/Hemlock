open! Basis.Rudiments
open! Basis
open Either
open Format

let test () =
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "swap %a -> %a@\n"
          (pp Uns.pp Uns.pp) either (pp Uns.pp Uns.pp) (swap either);
        fn eithers'
      end
  in
  let eithers = [
    First 0;
    Second 0;
  ] in
  fn eithers;
  printf "@]"

let _ = test ()
