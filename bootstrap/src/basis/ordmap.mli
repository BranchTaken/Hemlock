(** Ordered map. *)

type ('k, 'v, 'cmp) t

include Map_intf.S_ord with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
