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

  callbacks: Callbacks.t;
  (** Reduction callbacks. *)

  states: State.t array;
  (** Generated states. *)
}

val synthetic_name_of_start_name: string -> string
(** [synthetic_name_of_start_name start_name] returns a synthetic symbol name based on [start_name],
    e.g. "Start" -> "Start'". *)

val init: Conf.algorithm -> resolve:bool -> gc:Conf.gc -> remerge:Conf.remerge -> warn:bool -> Io.t
  -> Parse.nonterm_hmh -> Io.t * t
(** [init algorithm ~resolve ~gc ~remerge ~warn io hmh] creates a specification using the specified
    [algorithm] on [hmh], with conflicts optionally resolved, unreachable states optionally
    garbage-collected, functionally equivalent state subgraphs optionally remerged, unused grammar
    constructs optionally warned about, and all resulting I/O based on [io]. *)

val conflicts: t -> uns
(** [conflicts t] returns the number of grammar conflicts in [t]. *)
