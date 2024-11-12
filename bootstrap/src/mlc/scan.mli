(** Scanner. *)

open Basis
open Basis.Rudiments

module Source = Hmc.Source

module Token : sig
  module Rendition = Hmc.Scan.Token.Rendition
  type t =
    (* Keywords. *)
    | Tok_and of {source: Source.Slice.t}
    | Tok_as of {source: Source.Slice.t}
    | Tok_begin of {source: Source.Slice.t}
    | Tok_else of {source: Source.Slice.t}
    | Tok_end of {source: Source.Slice.t}
    | Tok_external of {source: Source.Slice.t}
    | Tok_false of {source: Source.Slice.t}
    | Tok_fun of {source: Source.Slice.t}
    | Tok_function of {source: Source.Slice.t}
    | Tok_if of {source: Source.Slice.t}
    | Tok_import of {source: Source.Slice.t}
    | Tok_in of {source: Source.Slice.t}
    | Tok_include of {source: Source.Slice.t}
    | Tok_lazy of {source: Source.Slice.t}
    | Tok_let of {source: Source.Slice.t}
    | Tok_match of {source: Source.Slice.t}
    | Tok_mod of {source: Source.Slice.t}
    | Tok_module of {source: Source.Slice.t}
    | Tok_mutable of {source: Source.Slice.t}
    | Tok_nonrec of {source: Source.Slice.t}
    | Tok_of of {source: Source.Slice.t}
    | Tok_open of {source: Source.Slice.t}
    | Tok_or of {source: Source.Slice.t}
    | Tok_rec of {source: Source.Slice.t}
    | Tok_sig of {source: Source.Slice.t}
    | Tok_struct of {source: Source.Slice.t}
    | Tok_then of {source: Source.Slice.t}
    | Tok_true of {source: Source.Slice.t}
    | Tok_type of {source: Source.Slice.t}
    | Tok_val of {source: Source.Slice.t}
    | Tok_when of {source: Source.Slice.t}
    | Tok_with of {source: Source.Slice.t}

    (* Operators. *)
    | Tok_tilde_op of {source: Source.Slice.t; tilde_op: string}
    | Tok_qmark_op of {source: Source.Slice.t; qmark_op: string}
    | Tok_star_star_op of {source: Source.Slice.t; star_star_op: string}
    | Tok_star_op of {source: Source.Slice.t; star_op: string}
    | Tok_slash_op of {source: Source.Slice.t; slash_op: string}
    | Tok_pct_op of {source: Source.Slice.t; pct_op: string}
    | Tok_plus_op of {source: Source.Slice.t; plus_op: string}
    | Tok_minus_op of {source: Source.Slice.t; minus_op: string}
    | Tok_at_op of {source: Source.Slice.t; at_op: string}
    | Tok_caret_op of {source: Source.Slice.t; caret_op: string}
    | Tok_dollar_op of {source: Source.Slice.t; dollar_op: string}
    | Tok_lt_op of {source: Source.Slice.t; lt_op: string}
    | Tok_eq_op of {source: Source.Slice.t; eq_op: string}
    | Tok_gt_op of {source: Source.Slice.t; gt_op: string}
    | Tok_bar_op of {source: Source.Slice.t; bar_op: string}
    | Tok_colon_op of {source: Source.Slice.t; colon_op: string}
    | Tok_dot_op of {source: Source.Slice.t; dot_op: string}

    (* Punctuation. *)
    | Tok_tilde of {source: Source.Slice.t}
    | Tok_qmark of {source: Source.Slice.t}
    | Tok_plus of {source: Source.Slice.t}
    | Tok_minus of {source: Source.Slice.t}
    | Tok_lt of {source: Source.Slice.t}
    | Tok_lt_eq of {source: Source.Slice.t}
    | Tok_eq of {source: Source.Slice.t}
    | Tok_lt_gt of {source: Source.Slice.t}
    | Tok_gt_eq of {source: Source.Slice.t}
    | Tok_gt of {source: Source.Slice.t}
    | Tok_comma of {source: Source.Slice.t}
    | Tok_dot of {source: Source.Slice.t}
    | Tok_dot_dot of {source: Source.Slice.t}
    | Tok_semi of {source: Source.Slice.t}
    | Tok_colon of {source: Source.Slice.t}
    | Tok_colon_colon of {source: Source.Slice.t}
    | Tok_colon_eq of {source: Source.Slice.t}
    | Tok_lparen of {source: Source.Slice.t}
    | Tok_rparen of {source: Source.Slice.t}
    | Tok_lbrack of {source: Source.Slice.t}
    | Tok_rbrack of {source: Source.Slice.t}
    | Tok_lcurly of {source: Source.Slice.t}
    | Tok_rcurly of {source: Source.Slice.t}
    | Tok_bar of {source: Source.Slice.t}
    | Tok_bar_bar of {source: Source.Slice.t}
    | Tok_larray of {source: Source.Slice.t}
    | Tok_rarray of {source: Source.Slice.t}
    | Tok_bslash of {source: Source.Slice.t}
    | Tok_tick of {source: Source.Slice.t}
    | Tok_caret of {source: Source.Slice.t}
    | Tok_amp_amp of {source: Source.Slice.t}
    | Tok_xmark of {source: Source.Slice.t}
    | Tok_arrow of {source: Source.Slice.t}

    (* Miscellaneous. *)
    | Tok_whitespace of {source: Source.Slice.t}
    | Tok_paren_comment of {source: Source.Slice.t; paren_comment: unit Rendition.t}
    | Tok_uscore of {source: Source.Slice.t}
    | Tok_uident of {source: Source.Slice.t; uident: string Rendition.t}
    | Tok_cident of {source: Source.Slice.t; cident: string}
    | Tok_char of {source: Source.Slice.t; char: codepoint Rendition.t}
    | Tok_qstring of {source: Source.Slice.t; qstring: string Rendition.t}
    | Tok_istring of {source: Source.Slice.t; istring: string Rendition.t}
    | Tok_r64 of {source: Source.Slice.t; r64: real Rendition.t}
    | Tok_long of {source: Source.Slice.t; long: u64 Rendition.t}
    | Tok_end_of_input of {source: Source.Slice.t}
    | Tok_error of {source: Source.Slice.t; error: Rendition.Malformation.t list}

  val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

  val source: t -> Source.Slice.t

  val malformations: t -> Rendition.Malformation.t list
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

val cursor: t -> Source.Cursor.t
(** [cursor t] returns the cursor at the scanner's current position. This cursor is equivalent to
    the base of the token returned by [next t]. *)

val next: t -> t * Token.t
(** [next t] scans the next token past the tokens scanned by [t]'s predecessor state(s) and returns
    the scanner's successor state along with a token. If [t] is at the end of input, there is no
    successor state, and [t, EndOfInput] is returned. *)
