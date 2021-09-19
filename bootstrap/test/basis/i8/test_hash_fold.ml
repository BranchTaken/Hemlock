open! Basis.Rudiments
open! Basis
open I8
open Format

let test () =
  printf "@[<h>";
  let rec test_hash_fold xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x x Hash.pp (Hash.t_of_state (hash_fold x Hash.State.empty));
        test_hash_fold xs'
      end
  end in
  let xs = [min_value; neg_one; zero; one; max_value] in
  test_hash_fold xs;
  printf "@]"

let _ = test ()
