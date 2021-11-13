open! Basis.Rudiments
open! Basis
open Hmc.Realer

let test () =
  let rec test_hash_fold rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> pp r
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold r Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold rs'
      end
  end in
  let rs = [zero; one; inf; nan; neg zero; neg one; neg inf] in
  test_hash_fold rs

let _ = test ()
