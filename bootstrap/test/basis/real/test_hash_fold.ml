open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let rec test_hash_fold reals = begin
    match reals with
    | [] -> ()
    | x :: reals' -> begin
        printf "hash_fold %h -> %a\n"
          x Hash.pp (Hash.t_of_state (hash_fold x Hash.State.empty));
        test_hash_fold reals'
      end
  end in
  let reals = [0.; 1.; 42.; infinity] in
  test_hash_fold reals;
  printf "@]"

let _ = test ()
