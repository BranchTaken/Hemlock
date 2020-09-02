(** Functors for integers of variable or constrained wordwidth. *)

open IntwIntf

(** Functor for variable-wordwidth signed integers. *)
module MakeVI (T : IV) : SVI with type t := T.t

(** Functor for variable-width unsigned integers. *)
module MakeVU (T : IV) : SVU with type t := T.t

(** Functor for fixed-wordwidth signed integers. *)
module MakeFI (T : IF) : SFI with type t := T.t

(** Functor for fixed-wordwidth unsigned integers. *)
module MakeFU (T : IF) : SFU with type t := T.t
