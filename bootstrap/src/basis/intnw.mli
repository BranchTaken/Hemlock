(** Functors for integers of a specific power-of-two wordwidth. *)

open IntnwIntf

(** Functor for signed integers of specific power-of-two wordwidth. *)
module MakeI (T : I) : SI with type t := T.t

(** Functor for unsigned integers of specific power-of-two wordwidth. *)
module MakeU (T : I) : SU with type t := T.t
