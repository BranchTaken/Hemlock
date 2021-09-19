open! Basis.Rudiments
open! Basis
open Basis.Array
open Format

let test () =
  printf "@[<h>";
  let rec fn arrs = begin
    match arrs with
    | [] -> ()
    | arr :: arrs' -> begin
        printf "hash_fold %a -> %a\n"
          (pp Uns.pp) arr
          Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold arr Hash.State.empty));
        fn arrs'
      end
  end in
  let arrs = [
    [||];
    [|0|];
    [|0; 0|];
    [|0; 1|]
  ] in
  fn arrs;
  printf "@]"

let _ = test ()
