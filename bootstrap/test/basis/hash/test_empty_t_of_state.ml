open! Basis.Rudiments
open! Basis
open Hash
open Format

let test () =
  printf "@[<h>";
  printf "hash=%a\n" xpp (t_of_state State.empty);
  printf "state=%a\n" State.xpp State.empty;
  printf "@]"

let _ = test ()
