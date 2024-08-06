(** Hemlock/OCaml code generation. *)

val generate_hmi: Conf.t -> Parse.hmhi -> Io.t -> Spec.t -> Io.t
(** [generate_hmi conf hmhi io spec] integrates a Hemlock interface (.hmi) representation of [spec]
    into [io].
*)

val generate_hm: Conf.t -> Parse.hmh -> Io.t -> Spec.t -> Io.t
(** [generate_hm conf hmh io spec] integrates a Hemlock (.hm) representation of [spec] into [io]. *)

val generate_mli: Conf.t -> Parse.hmhi -> Io.t -> Spec.t -> Io.t
(** [generate_mli conf hmhi io spec] integrates an OCaml interface (.mli) representation of [spec]
    into [io]. *)

val generate_ml: Conf.t -> Parse.hmh -> Io.t -> Spec.t -> Io.t
(** [generate_ml conf hmh io spec] integrates an OCaml (.ml) representation of [spec] into [io]. *)
