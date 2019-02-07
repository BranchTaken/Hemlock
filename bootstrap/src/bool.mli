type t = bool [@@deriving compare]

include Identifiable_intf.S with type t := t
include Intable_intf.S with type t := t

val not: t -> t

(* The && and || operators built in to OCaml are magically short-circuiting.
 * If we define equivalent operators they lose the short-circuiting magic. *)
(*
val ( && ): t -> t -> t
val ( || ): t -> t -> t
*)
