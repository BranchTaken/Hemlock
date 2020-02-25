(** Functor for identifiable types. *)

open Identifiable_intf

(** Functor for identifiable types. *)
module Make (T : I) : S with type t := T.t
