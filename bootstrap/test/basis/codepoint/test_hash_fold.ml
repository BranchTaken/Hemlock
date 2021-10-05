open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let test () =
  printf "@[<h>";
  let rec test_hash_fold cps = begin
    match cps with
    | [] -> ()
    | cp :: cps' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x (extend_to_uns cp) Hash.pp (Hash.t_of_state (hash_fold cp Hash.State.empty));
        test_hash_fold cps'
      end
  end in
  let cps = [nul; soh; del; replacement; kv 0x10_ffffL] in
  test_hash_fold cps;
  printf "@]"

let _ = test ()
