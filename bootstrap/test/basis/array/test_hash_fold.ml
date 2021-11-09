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
          (xpp Uns.xpp) arr
          Hash.xpp (Hash.t_of_state (hash_fold Uns.hash_fold arr Hash.State.empty));
        fn arrs'
      end
  end in
  let arrs = [
    [||];
    [|0L|];
    [|0L; 0L|];
    [|0L; 1L|]
  ] in
  fn arrs;
  printf "@]"

let _ = test ()
