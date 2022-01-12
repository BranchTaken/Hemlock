open! Basis.Rudiments
open! Basis
open Nat

let test () =
  let rec test_hash_fold us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> fmt ~alt:true ~base:Fmt.Hex u
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold u Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold us'
      end
  end in
  let us = [zero; one] in
  test_hash_fold us

let _ = test ()
