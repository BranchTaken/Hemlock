open! Basis.Rudiments
open! Basis
open! SetTest
open Set
open Format

let test () =
  printf "@[";
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let set = of_list (module Uns) l in
        printf "hash_fold (of_list (module Uns) %a) -> %a@\n"
          (List.pp Uns.pp) l
          Hash.pp (Hash.t_of_state (hash_fold set Hash.State.empty));
        fn lists'
      end
  in
  let lists = [
    [];
  ] in
  fn lists;
  printf "@]"

let _ = test ()
