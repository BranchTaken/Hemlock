open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | option :: options' -> begin
        printf "hash_fold %a -> %a\n"
          (pp Uns.pp) option
          Hash.pp (Hash.t_of_state
            (hash_fold Uns.hash_fold option Hash.State.empty));
        fn options'
      end
  in
  let options = [
    None;
    (Some 0L);
    (Some 1L)
  ] in
  fn options;
  printf "@]"

let _ = test ()
