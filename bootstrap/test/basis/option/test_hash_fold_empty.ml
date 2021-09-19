open! Basis.Rudiments
open! Basis
open Option

let test () =
  let hash_empty state = begin
    state
    |> hash_fold Unit.hash_fold None
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
