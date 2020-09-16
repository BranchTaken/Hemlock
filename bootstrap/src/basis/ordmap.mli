(** Ordered map. *)

type ('k, 'v, 'cmp) t

include MapIntf.SOrd with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t
