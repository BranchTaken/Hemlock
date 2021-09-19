open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[";
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let map = of_klist l in
        printf "hash_fold (of_klist %a) -> %a@\n"
          (List.pp Uns.pp) l
          Hash.pp (Hash.t_of_state
            (hash_fold Uns.hash_fold map Hash.State.empty));
        fn lists'
      end
  in
  (* NB: [0; 1] and [0; 2] collide. This is because we're using UnsTestCmper to get stable test
   * output; the hashing results from all but the last binding hashed are discarded. *)
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 2];
    [2; 3]
  ] in
  fn lists;
  printf "@]"

let _ = test ()
