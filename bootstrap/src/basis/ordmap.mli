(** Ordered map. *)

type ('k, 'v, 'cmp) t

include MapIntf.SOrd with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t

(* Exposed for testing purposes only. *)
val pp: (Format.formatter -> 'v -> unit) -> Format.formatter -> ('k, 'v, 'cmp) t -> unit
val pp_kv: (Format.formatter -> 'v -> unit) -> Format.formatter -> Uns.t * 'v -> unit
val cursor_pp: Format.formatter -> ('k, 'v, 'cmp) Cursor.t -> unit
val validate: ('k, 'v, 'cmp) t -> unit
