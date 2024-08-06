(** Text/html description generation. *)

val generate_txt: Conf.t -> Io.t -> Spec.t -> Io.t
(** [generate_txt conf io spec] integrates a text representation of [spec] into [io]. *)

val generate_html: Conf.t -> Io.t -> Spec.t -> Io.t
(** [generate_html conf io spec] integrates an html representation of [spec] into [io]. *)
