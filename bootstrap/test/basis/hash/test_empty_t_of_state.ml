open! Basis.Rudiments
open! Basis
open Hash
open Format

let test () =
  printf "@[<h>";
  printf "hash=%a\n" pp (t_of_state State.empty);
  printf "state=%a\n" State.pp State.empty;
  printf "@]"

let _ = test ()
