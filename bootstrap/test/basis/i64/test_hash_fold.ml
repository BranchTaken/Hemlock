open! Basis.Rudiments
open! Basis
open I64

let test () =
  let rec test_hash_fold xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold x Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold xs'
      end
  end in
  let us = [zero; one; min_value; max_value] in
  test_hash_fold us

let _ = test ()
