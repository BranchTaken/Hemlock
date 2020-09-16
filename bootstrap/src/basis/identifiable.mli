(** Functor for identifiable types. *)

open IdentifiableIntf

(** Functor for identifiable types. *)
module Make (T : I) : S with type t := T.t
