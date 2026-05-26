(** Hocc/Yacc grammar generation. *)

val generate_hocc: Io.t -> Spec.t -> Io.t
(** [generate_hocc io spec] integrates a Hocc representation of [spec]'s grammar into [io]. *)

val generate_yacc: Io.t -> Spec.t -> Io.t
(** [generate_yacc io spec] integrates a Yacc representation of [spec]'s grammar into [io]. *)
