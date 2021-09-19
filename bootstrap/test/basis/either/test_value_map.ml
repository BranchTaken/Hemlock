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
          (pp Uns.pp Uns.pp) either Uns.pp
          (value_map ~first:(fun x -> x + 2) ~second:(fun x -> x + 4) either);
        fn eithers'
      end
  in
  let eithers = [
    First 1;
    Second 2;
  ] in
  fn eithers;
  printf "@]"

let _ = test ()
