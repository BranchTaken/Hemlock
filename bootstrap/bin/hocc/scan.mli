(** Thin wrapper around Hmc's scanner that adds hocc-specific keywords. *)

open Basis
open! Basis.Rudiments

module Token: sig
  type t =
    | HmcToken of Hmc.Scan.Token.t
    | Tok_hocc of {source: Hmc.Source.Slice.t}
    | Tok_token of {source: Hmc.Source.Slice.t}
    | Tok_nonterm of {source: Hmc.Source.Slice.t}
    | Tok_start of {source: Hmc.Source.Slice.t}
    | Tok_epsilon of {source: Hmc.Source.Slice.t}
    | Tok_neutral of {source: Hmc.Source.Slice.t}
    | Tok_left of {source: Hmc.Source.Slice.t}
    | Tok_right of {source: Hmc.Source.Slice.t}
    | Tok_prec of {source: Hmc.Source.Slice.t}
    | Tok_colon_colon_eq of {source: Hmc.Source.Slice.t}

  val source: t -> Hmc.Source.Slice.t

  include FormattableIntf.SMono with type t := t

  val malformations: t -> Hmc.Scan.Token.Rendition.Malformation.t list
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
