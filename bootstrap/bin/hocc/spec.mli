(** Hocc specification. *)

open! Basis
open! Basis.Rudiments

type t = {
  algorithm: Conf.algorithm;
  (** Algorithm used to generate states. *)

  precs: Precs.t;
  (** Precedences. *)

  symbols: Symbols.t;
  (** Symbols. *)

  prods: Prods.t;
  (** Productions. *)

  reductions: Reductions.t;
  (** Reductions. *)

  states: State.t array;
  (** Generated states. *)
}

val init: Conf.algorithm -> resolve:bool -> Io.t -> Parse.hmh -> Io.t * t
(** [init algorithm ~resolve io hmh] creates a specification using the specified [algorithm] on
    [hmh], with conflicts optionally resolved, and all resulting I/O based on [io]. *)

val conflicts: t -> uns
(** [conflicts t] returns the number of grammar conflicts in [t]. *)

val to_txt: Conf.t -> Io.t -> t -> Io.t
(** [to_txt conf io t] integrates a text representation of [t] into [io]. *)

val to_html: Conf.t -> Io.t -> t -> Io.t
(** [to_html conf io t] integrates an html representation of [t] into [io]. *)

val to_hocc: Io.t -> t -> Io.t
(** [to_hocc conf io t] integrates a hocc representation of [t]'s grammar into [io]. States are
    omitted since they have no hocc representation. *)

val to_hmi: Conf.t -> Parse.hmhi -> Io.t -> t -> Io.t
(** [to_hmi conf hmhi io t] integrates a Hemlock interface (.hmi) representation of [t] into [io].
*)

val to_hm: Conf.t -> Parse.hmh -> Io.t -> t -> Io.t
(** [to_hm conf hmh io t] integrates a Hemlock (.hm) representation of [t] into [io]. *)

val to_mli: Conf.t -> Parse.hmhi -> Io.t -> t -> Io.t
(** [to_mli conf hmhi io t] integrates an OCaml interface (.mli) representation of [t] into [io]. *)

val to_ml: Conf.t -> Parse.hmh -> Io.t -> t -> Io.t
(** [to_ml conf hmh io t] integrates an OCaml (.ml) representation of [t] into [io]. *)
