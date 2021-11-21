open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec test_hash_fold reals = begin
    match reals with
    | [] -> ()
    | x :: reals' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> fmt ~alt:true ~base:Fmt.Hex x
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold x Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold reals'
      end
  end in
  let reals = [0.; 1.; 42.; infinity] in
  test_hash_fold reals

let _ = test ()
