(** Ordered map. *)

type ('k, 'v, 'cmp) t

include MapIntf.SOrd with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) t

(* Exposed for testing purposes only. *)
val fmt: ?alt:bool -> ?width:int64 -> ('v -> (module Fmt.Formatter) -> (module Fmt.Formatter))
  -> ('k, 'v, 'cmp) t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
val validate: ('k, 'v, 'cmp) t -> unit
