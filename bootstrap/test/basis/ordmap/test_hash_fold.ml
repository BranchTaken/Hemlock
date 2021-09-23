open! Basis.Rudiments
open! Basis
open OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let ordmap = of_klist l in
        printf "hash_fold (of_klist %a) -> %a@\n"
          (List.pp Uns.pp) l
          Hash.pp (Hash.t_of_state
            (hash_fold Uns.hash_fold ordmap Hash.State.empty));
        fn lists'
      end
  in
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 2L];
    [2L; 3L]
  ] in
  fn lists;
  printf "@]"

let _ = test ()
