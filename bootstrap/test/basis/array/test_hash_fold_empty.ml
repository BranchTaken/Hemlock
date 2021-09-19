open! Basis.Rudiments
open! Basis
open Array

let test () =
  let hash_empty state = begin
    state
    |> hash_fold Unit.hash_fold [||]
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
  assert Cmp.(is_ne (U128.cmp (Hash.t_of_state e1) (Hash.t_of_state e2)))

let _ = test ()
