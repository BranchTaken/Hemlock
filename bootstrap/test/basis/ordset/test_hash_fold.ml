open! Basis.Rudiments
open! Basis
open! OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  let rec fn = function
    | [] -> ()
    | arr :: arrs' -> begin
        let ordset = of_array (module Uns) arr in
        printf "hash_fold (of_array (module Uns) %a) -> %a@\n"
          (Array.xpp Uns.xpp) arr
          Hash.xpp (Hash.t_of_state (hash_fold ordset Hash.State.empty));
        fn arrs'
      end
  in
  let arrs = [
    [||];
    [|0L|];
    [|0L; 1L|];
    [|0L; 2L|]
  ] in
  fn arrs;
  printf "@]"

let _ = test ()
