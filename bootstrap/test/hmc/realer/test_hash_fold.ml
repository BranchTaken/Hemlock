open! Basis.Rudiments
open! Basis
open Hmc.Realer
open Format

let test () =
  printf "@[<h>";
  let rec test_hash_fold rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        printf "hash_fold %a -> %a\n"
          xpp r Hash.xpp (Hash.t_of_state (hash_fold r Hash.State.empty));
        test_hash_fold rs'
      end
  end in
  let rs = [zero; one; inf; nan; neg zero; neg one; neg inf] in
  test_hash_fold rs;
  printf "@]"

let _ = test ()
