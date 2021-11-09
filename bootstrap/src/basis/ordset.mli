(** Ordered set. *)

type ('a, 'cmp) t

include SetIntf.SOrd with type ('a, 'cmp) t := ('a, 'cmp) t

(* Exposed for testing purposes only. *)
val cursor_xpp: Format.formatter -> ('a, 'cmp) Cursor.t -> unit
val cursor_fmt: ('a, 'cmp) Cursor.t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
