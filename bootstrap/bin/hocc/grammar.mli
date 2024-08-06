(** hocc grammar generation. *)

val generate_hocc: Io.t -> Spec.t -> Io.t
(** [generate_hocc conf io spec] integrates a hocc representation of [spec]'s grammar into [io]. *)
