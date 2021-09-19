open! Basis.Rudiments
open! Basis
open String

let test () =
  let hash_empty state = begin
    state
    |> hash_fold ""
  end in
  let e1 =
    Hash.State.empty
    |> hash_empty
  in
  let e2 =
    Hash.State.empty
    |> hash_empty
    |> hash_empty
  in
  assert U128.((Hash.t_of_state e1) <> (Hash.t_of_state e2))

let _ = test ()
