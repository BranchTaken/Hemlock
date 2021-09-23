open! Basis.Rudiments
open! Basis
open Either
open Format

let test () =
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        printf "hash_fold %a -> %a\n"
          (pp Uns.pp Uns.pp) either
          Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold Uns.hash_fold
              either Hash.State.empty));
        fn eithers'
      end
  in
  let eithers = [
    First 0L;
    First 1L;
    Second 0L;
    Second 1L;
  ] in
  fn eithers;
  printf "@]"

let _ = test ()
