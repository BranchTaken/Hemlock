(** Ordered map. *)

type ('k, 'v, 'cmp) t

include MapIntf.SOrd with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t

(* Exposed for testing purposes only. *)
val pp: (Format.formatter -> 'v -> unit) -> Format.formatter -> ('k, 'v, 'cmp) t -> unit
val fmt: ('v -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> ('k, 'v, 'cmp) t
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val pp_kv: (Format.formatter -> 'v -> unit) -> Format.formatter -> Uns.t * 'v -> unit
val fmt_kv: ('v -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> Uns.t * 'v
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val cursor_pp: Format.formatter -> ('k, 'v, 'cmp) Cursor.t -> unit
val cursor_fmt: ('k, 'v, 'cmp) Cursor.t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val validate: ('k, 'v, 'cmp) t -> unit
