(** Functors for integers of specific bitwidth. *)

open IntnbIntf

(** Functor for derived functions on an integer of specific bitwidth. *)
module MakeDerived (T : IDerived) : SDerived with type t := T.t

(** Functor for signed integers of specific bitwidth. *)
module MakeI (_ : I) : SI with type t := sint

(** Functor for unsigned integers of specific bitwidth. *)
module MakeU (_ : I) : SU with type t := uns
