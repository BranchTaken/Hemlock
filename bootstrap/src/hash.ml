open Rudiments_int0

type t = usize
type state = usize

let state_of_usize x =
  x

let t_of_state state =
  state

let pp ppf t =
  Format.fprintf ppf "0x%016x" t

let hash_fold state a =
  Hashtbl.seeded_hash state a

let hash a =
  Hashtbl.hash a
