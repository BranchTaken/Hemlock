open! Basis.Rudiments
open! Basis
open Z
open Format

let test () =
  printf "@[<h>";
  let rec test_hash_fold us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x u Hash.pp (Hash.t_of_state (hash_fold u Hash.State.empty));
        test_hash_fold us'
      end
  end in
  let us = [zero; one] in
  test_hash_fold us;
  printf "@]"

let _ = test ()
