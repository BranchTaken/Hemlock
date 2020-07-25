(** Functors for integers of a specific power-of-two wordwidth. *)

open Intnw_intf

(** Functor for signed integers of specific power-of-two wordwidth. *)
module Make_i (T : I) : S_i with type t := T.t

(** Functor for unsigned integers of specific power-of-two wordwidth. *)
module Make_u (T : I) : S_u with type t := T.t
