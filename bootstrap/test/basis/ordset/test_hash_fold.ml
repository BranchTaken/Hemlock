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
          (Array.pp Uns.pp) arr
          Hash.pp (Hash.t_of_state (hash_fold ordset Hash.State.empty));
        fn arrs'
      end
  in
  let arrs = [
    [||];
    [|0|];
    [|0; 1|];
    [|0; 2|]
  ] in
  fn arrs;
  printf "@]"

let _ = test ()
