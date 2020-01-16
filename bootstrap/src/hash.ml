open Rudiments_uint0

type t = uint
type state = uint

let state_of_uint x =
  x

let t_of_state state =
  state

let s_fmt () t =
  Printf.sprintf "%u" (int_of_uint t)

let s_fmt_hex () t =
  Printf.sprintf "0x%x" (int_of_uint t)

let fmt oc t =
  output_string oc (s_fmt () t)

let fmt_hex oc t =
  output_string oc (s_fmt_hex () t)

let hash_fold state a =
  uint_of_int (Hashtbl.seeded_hash (int_of_uint state) a)

let hash a =
  uint_of_int (Hashtbl.hash a)
