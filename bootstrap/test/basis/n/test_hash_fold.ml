open! Basis.Rudiments
open! Basis
open N
open Format

let test () =
  printf "@[<h>";
  let rec test_hash_fold us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        printf "hash_fold %a -> %a\n"
          xpp_x u Hash.xpp (Hash.t_of_state (hash_fold u Hash.State.empty));
        test_hash_fold us'
      end
  end in
  let us = [zero; one] in
  test_hash_fold us;
  printf "@]"

let _ = test ()
