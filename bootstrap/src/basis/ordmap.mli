(** Ordered map. *)

type ('k, 'v, 'cmp) t

include MapIntf.SOrd with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t

(* Exposed for testing purposes only. *)
val xpp: (Format.formatter -> 'v -> unit) -> Format.formatter -> ('k, 'v, 'cmp) t -> unit
val pp: ('v -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> ('k, 'v, 'cmp) t
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val xpp_kv: (Format.formatter -> 'v -> unit) -> Format.formatter -> Uns.t * 'v -> unit
val pp_kv: ('v -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> Uns.t * 'v
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val cursor_xpp: Format.formatter -> ('k, 'v, 'cmp) Cursor.t -> unit
val cursor_pp: ('k, 'v, 'cmp) Cursor.t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val validate: ('k, 'v, 'cmp) t -> unit
