open! Basis.Rudiments
open! Basis
open Bool
open Format

let test () =
  printf "@[<h>";
  let rec test_hash_fold bools = begin
    match bools with
    | [] -> ()
    | b :: bools' -> begin
        printf "hash_fold %a -> %a\n"
          pp b Hash.pp (Hash.t_of_state (hash_fold b Hash.State.empty));
        test_hash_fold bools'
      end
  end in
  let bools = [false; true] in
  test_hash_fold bools;
  printf "@]"

let _ = test ()
