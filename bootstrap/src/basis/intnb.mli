(** Functors for integers of specific bitwidth. *)

open Intnb_intf

(** Functor for derived functions on an integer of specific bandwidth. *)
module Make_derived (T : I_derived) : S_derived with type t := T.t

(** Functor for signed integers of specific bitwidth. *)
module Make_i (T : I) : S_i with type t := int

(** Functor for unsigned integers of specific bitwidth. *)
module Make_u (T : I) : S_u with type t := uns
