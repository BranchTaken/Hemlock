(** Thin wrapper around Hmc's scanner that adds hocc-specific keywords. *)

open Basis
open! Basis.Rudiments

module AbstractToken: sig
  type t =
    | Tok_hocc
    | Tok_token
    | Tok_nonterm
    | Tok_start
    | Tok_epsilon
    | Tok_neutral
    | Tok_left
    | Tok_right
    | Tok_prec

  val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

  val malformations: t -> Hmc.Scan.AbstractToken.Rendition.Malformation.t list
  (** [malformations t] returns a list of malformations associated with [t], or an empty list if
      there are no malformations. This function can be used on any token variant, even if no
      malformations are possible. *)
end

module ConcreteToken : sig
  type t = {
    atok: AbstractToken.t;
    source: Hmc.Source.Slice.t;
  }

  val atok: t -> AbstractToken.t
  val source: t -> Hmc.Source.Slice.t

  include FormattableIntf.SMono with type t := t
end

module Token: sig
  type t =
    | HmcToken of Hmc.Scan.ConcreteToken.t
    | HoccToken of ConcreteToken.t

  val source: t -> Hmc.Source.Slice.t

  include FormattableIntf.SMono with type t := t

  val malformations: t -> Hmc.Scan.AbstractToken.Rendition.Malformation.t list
  (** [malformations t] returns a list of malformations associated with [t], or an empty list if
      there are no malformations. This function can be used on any token variant, even if no
      malformations are possible. *)
end

type t

include FormattableIntf.SMono with type t := t

val init: Text.t -> t
(** [init text] initializes scanner to scan [text]. *)

val text: t -> Text.t
(** [text t] returns the source text for [t]. *)

val cursor: t -> Hmc.Source.Cursor.t
(** [cursor t] returns the cursor at the scanner's current position. This cursor is equivalent to
    the base of the token returned by [next t]. *)

val next: t -> t * Token.t
(** [next t] scans the next token past the tokens scanned by [t]'s predecessor state(s) and returns
    the scanner's successor state along with a token. If [t] is at the end of input, there is no
    successor state, and [t, (HmcToken EndOfInput)] is returned. *)
