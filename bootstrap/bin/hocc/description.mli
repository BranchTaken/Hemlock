(** Text description generation. *)

val generate_txt: Conf.t -> Io.t -> Spec.t -> Io.t
(** [generate_txt conf io spec] integrates a text representation of [spec] into [io]. *)
