open Rudiments_uint0

type t = uint
type state = uint

let state_of_uint x =
  x

let t_of_state state =
  state

let pp ppf t =
  Format.fprintf ppf "%u" (int_of_uint t)

let pp_x ppf t =
  Format.fprintf ppf "0x%x" (int_of_uint t)

let hash_fold state a =
  uint_of_int (Hashtbl.seeded_hash (int_of_uint state) a)

let hash a =
  uint_of_int (Hashtbl.hash a)
