(** Scanner. *)

(* The following naming convention is used for special codepoints which show up in the language
   syntax. Token names for other codepoints are e.g. Tok_a and Tok_A; concatenations are e.g.
   Tok_aA. The special codepoints are referred to similarly, e.g. Tok_nl and Tok_lbrack_bar.

   - '\t' : ht
   - '\n' : nl
   - '\r' : cr
   - ' '  : space
   - '!'  : xmark
   - '"'  : ditto
   - '#'  : hash
   - '$'  : dollar
   - '%'  : pct
   - '&'  : amp
   - '\'' : tick
   - '('  : lparen
   - ')'  : rparen
   - '*'  : star
   - '+'  : plus
   - ','  : comma
   - '-'  : minus
   - '.'  : dot
   - '/'  : slash
   - ':'  : colon
   - ';'  : semi
   - '<'  : lt
   - '='  : eq
   - '>'  : gt
   - '?'  : qmark
   - '@'  : at
   - '['  : lbrack
   - '\\' : bslash
   - ']'  : rbrack
   - '^'  : caret
   - '_'  : uscore
   - '`'  : btick
   - '{'  : lcurly
   - '|'  : bar
   - '}'  : rcurly
   - '~'  : tilde
*)

open Basis
open Basis.Rudiments

module Token : sig
  module Rendition : sig
    module Malformation : sig
      type t

      include FormattableIntf.SMono with type t := t

      val of_source: source:Source.Slice.t -> description:string -> t
      val of_cursors: base:Source.Cursor.t -> past:Source.Cursor.t -> description:string -> t
      val source: t -> Source.Slice.t
      val description: t -> string
    end
    type 'a t =
      | Constant of 'a
      | Malformed of Malformation.t list

    include FormattableIntf.SPoly with type 'a t := 'a t
  end
  type indent_omit = {
    indent: uns;
    omit: uns;
  }
  type source_directive = {
    path: Path.t option;
    line: uns option;
    io: indent_omit option;
  }
  type fmt =
    | Fmt_b
    | Fmt_u
    | Fmt_u8
    | Fmt_u16
    | Fmt_u32
    | Fmt_u64
    | Fmt_u128
    | Fmt_u256
    | Fmt_u512
    | Fmt_i
    | Fmt_i8
    | Fmt_i16
    | Fmt_i32
    | Fmt_i64
    | Fmt_i128
    | Fmt_i256
    | Fmt_i512
    | Fmt_n
    | Fmt_z
    | Fmt_r
    | Fmt_r32
    | Fmt_r64
    | Fmt_c
    | Fmt_s
    | Fmt_f
  type t =
    (* Keywords. *)
    | Tok_and of {source: Source.Slice.t}
    | Tok_also of {source: Source.Slice.t}
    | Tok_as of {source: Source.Slice.t}
    | Tok_conceal of {source: Source.Slice.t}
    | Tok_effect of {source: Source.Slice.t}
    | Tok_else of {source: Source.Slice.t}
    | Tok_expose of {source: Source.Slice.t}
    | Tok_external of {source: Source.Slice.t}
    | Tok_false of {source: Source.Slice.t}
    | Tok_fn of {source: Source.Slice.t}
    | Tok_function of {source: Source.Slice.t}
    | Tok_if of {source: Source.Slice.t}
    | Tok_import of {source: Source.Slice.t}
    | Tok_include of {source: Source.Slice.t}
    | Tok_lazy of {source: Source.Slice.t}
    | Tok_let of {source: Source.Slice.t}
    | Tok_match of {source: Source.Slice.t}
    | Tok_mutability of {source: Source.Slice.t}
    | Tok_of of {source: Source.Slice.t}
    | Tok_open of {source: Source.Slice.t}
    | Tok_or of {source: Source.Slice.t}
    | Tok_rec of {source: Source.Slice.t}
    | Tok_then of {source: Source.Slice.t}
    | Tok_true of {source: Source.Slice.t}
    | Tok_type of {source: Source.Slice.t}
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
    | Tok_lcapture of {source: Source.Slice.t}
    | Tok_rcapture of {source: Source.Slice.t}
    | Tok_larray of {source: Source.Slice.t}
    | Tok_rarray of {source: Source.Slice.t}
    | Tok_bslash of {source: Source.Slice.t}
    | Tok_tick of {source: Source.Slice.t}
    | Tok_caret of {source: Source.Slice.t}
    | Tok_amp of {source: Source.Slice.t}
    | Tok_amp_amp of {source: Source.Slice.t}
    | Tok_xmark of {source: Source.Slice.t}
    | Tok_arrow of {source: Source.Slice.t}
    | Tok_carrow of {source: Source.Slice.t}

    (* Miscellaneous. *)
    | Tok_source_directive of
        {source: Source.Slice.t; source_directive: source_directive Rendition.t}
    | Tok_line_delim of {source: Source.Slice.t}
    | Tok_indent of {source: Source.Slice.t; indent: unit Rendition.t}
    | Tok_dedent of {source: Source.Slice.t; dedent: unit Rendition.t}
    | Tok_whitespace of {source: Source.Slice.t}
    | Tok_hash_comment of {source: Source.Slice.t}
    | Tok_paren_comment of {source: Source.Slice.t; paren_comment: unit Rendition.t}
    | Tok_uscore of {source: Source.Slice.t}
    | Tok_uident of {source: Source.Slice.t; uident: string Rendition.t}
    | Tok_cident of {source: Source.Slice.t; cident: string}
    | Tok_codepoint of {source: Source.Slice.t; codepoint: codepoint Rendition.t}
    | Tok_rstring of {source: Source.Slice.t; rstring: string Rendition.t}
    | Tok_qstring of {source: Source.Slice.t; qstring: string Rendition.t}
    | Tok_istring of {source: Source.Slice.t; istring: string Rendition.t}
    | Tok_fstring_lditto of {source: Source.Slice.t}
    | Tok_fstring_interpolated of {source: Source.Slice.t; fstring_interpolated: string Rendition.t}
    | Tok_fstring_pct of {source: Source.Slice.t}
    | Tok_fstring_pad of {source: Source.Slice.t; fstring_pad: codepoint Rendition.t}
    | Tok_fstring_just of {source: Source.Slice.t; fstring_just: Fmt.just}
    | Tok_fstring_sign of {source: Source.Slice.t; fstring_sign: Fmt.sign}
    | Tok_fstring_alt of {source: Source.Slice.t}
    | Tok_fstring_zpad of {source: Source.Slice.t}
    | Tok_fstring_width_star of {source: Source.Slice.t}
    | Tok_fstring_width of {source: Source.Slice.t; fstring_width: uns Rendition.t}
    | Tok_fstring_pmode of {source: Source.Slice.t; fstring_pmode: Fmt.pmode}
    | Tok_fstring_precision_star of {source: Source.Slice.t}
    | Tok_fstring_precision of {source: Source.Slice.t; fstring_precision: uns Rendition.t}
    | Tok_fstring_radix of {source: Source.Slice.t; fstring_radix: Radix.t}
    | Tok_fstring_notation of {source: Source.Slice.t; fstring_notation: Fmt.notation}
    | Tok_fstring_pretty of {source: Source.Slice.t}
    | Tok_fstring_fmt of {source: Source.Slice.t; fstring_fmt: fmt Rendition.t}
    | Tok_fstring_sep of {source: Source.Slice.t; fstring_sep: string Rendition.t}
    | Tok_fstring_label of {source: Source.Slice.t; fstring_label: string}
    | Tok_fstring_lparen_caret of {source: Source.Slice.t}
    | Tok_fstring_caret_rparen of {source: Source.Slice.t}
    | Tok_fstring_rditto of {source: Source.Slice.t}
    | Tok_r32 of {source: Source.Slice.t; r32: real Rendition.t}
    | Tok_r64 of {source: Source.Slice.t; r64: real Rendition.t}
    | Tok_u8 of {source: Source.Slice.t; u8: u8 Rendition.t}
    | Tok_i8 of {source: Source.Slice.t; i8: i8 Rendition.t}
    | Tok_u16 of {source: Source.Slice.t; u16: u16 Rendition.t}
    | Tok_i16 of {source: Source.Slice.t; i16: i16 Rendition.t}
    | Tok_u32 of {source: Source.Slice.t; u32: u32 Rendition.t}
    | Tok_i32 of {source: Source.Slice.t; i32: i32 Rendition.t}
    | Tok_long of {source: Source.Slice.t; long: u64 Rendition.t}
    | Tok_u64 of {source: Source.Slice.t; u64: u64 Rendition.t}
    | Tok_i64 of {source: Source.Slice.t; i64: i64 Rendition.t}
    | Tok_u128 of {source: Source.Slice.t; u128: u128 Rendition.t}
    | Tok_i128 of {source: Source.Slice.t; i128: i128 Rendition.t}
    | Tok_u256 of {source: Source.Slice.t; u256: u256 Rendition.t}
    | Tok_i256 of {source: Source.Slice.t; i256: i256 Rendition.t}
    | Tok_u512 of {source: Source.Slice.t; u512: u512 Rendition.t}
    | Tok_i512 of {source: Source.Slice.t; i512: i512 Rendition.t}
    | Tok_nat of {source: Source.Slice.t; nat: Nat.t Rendition.t}
    | Tok_zint of {source: Source.Slice.t; zint: Zint.t Rendition.t}
    | Tok_end_of_input of {source: Source.Slice.t}
    | Tok_misaligned of {source: Source.Slice.t}
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
