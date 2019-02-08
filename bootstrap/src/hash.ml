type t = int
type state = int

let state_of_int x =
  x

let t_of_state state =
  state

let hash_fold state a =
  Hashtbl.seeded_hash state a

let hash a =
  Hashtbl.hash a
