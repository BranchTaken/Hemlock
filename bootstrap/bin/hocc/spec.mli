(** Hocc specification. *)

open! Basis
open! Basis.Rudiments

type t = {
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
