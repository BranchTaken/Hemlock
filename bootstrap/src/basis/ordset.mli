(** Ordered set. *)

type ('a, 'cmp) t

include SetIntf.SOrd with type ('a, 'cmp) t := ('a, 'cmp) t

(* Exposed for testing purposes only. *)
val cursor_pp: Format.formatter -> ('a, 'cmp) Cursor.t -> unit
