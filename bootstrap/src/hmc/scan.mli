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

(** Abstract tokens do not have source location information, which makes it possible for ASTs to be
    recognized as equal even if there are code formatting differences between two sources. *)
module AbstractToken : sig
  module Rendition : sig
    module Malformation : sig
      type t

      include FormattableIntf.SMono with type t := t

      val init: base:Source.Cursor.t -> past:Source.Cursor.t -> description:string -> t
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
    | Tok_and
    | Tok_also
    | Tok_as
    | Tok_conceal
    | Tok_effect
    | Tok_else
    | Tok_expose
    | Tok_external
    | Tok_false
    | Tok_fn
    | Tok_function
    | Tok_if
    | Tok_import
    | Tok_include
    | Tok_lazy
    | Tok_let
    | Tok_match
    | Tok_mutability
    | Tok_of
    | Tok_open
    | Tok_or
    | Tok_rec
    | Tok_then
    | Tok_true
    | Tok_type
    | Tok_when
    | Tok_with

    (* Operators. *)
    | Tok_tilde_op of string
    | Tok_qmark_op of string
    | Tok_star_star_op of string
    | Tok_star_op of string
    | Tok_slash_op of string
    | Tok_pct_op of string
    | Tok_plus_op of string
    | Tok_minus_op of string
    | Tok_at_op of string
    | Tok_caret_op of string
    | Tok_dollar_op of string
    | Tok_lt_op of string
    | Tok_eq_op of string
    | Tok_gt_op of string
    | Tok_bar_op of string
    | Tok_colon_op of string
    | Tok_dot_op of string

    (* Punctuation. *)
    | Tok_tilde
    | Tok_qmark
    | Tok_minus
    | Tok_lt
    | Tok_lt_eq
    | Tok_eq
    | Tok_lt_gt
    | Tok_gt_eq
    | Tok_gt
    | Tok_comma
    | Tok_dot
    | Tok_dot_dot
    | Tok_semi
    | Tok_colon
    | Tok_colon_colon
    | Tok_colon_eq
    | Tok_lparen
    | Tok_rparen
    | Tok_lbrack
    | Tok_rbrack
    | Tok_lcurly
    | Tok_rcurly
    | Tok_bar
    | Tok_lcapture
    | Tok_rcapture
    | Tok_larray
    | Tok_rarray
    | Tok_bslash
    | Tok_tick
    | Tok_caret
    | Tok_amp
    | Tok_xmark
    | Tok_arrow
    | Tok_carrow

    (* Miscellaneous. *)
    | Tok_source_directive of source_directive Rendition.t
    | Tok_line_delim
    | Tok_indent of unit Rendition.t
    | Tok_dedent of unit Rendition.t
    | Tok_whitespace
    | Tok_hash_comment
    | Tok_paren_comment of unit Rendition.t
    | Tok_uscore
    | Tok_uident of string Rendition.t
    | Tok_cident of string
    | Tok_codepoint of codepoint Rendition.t
    | Tok_rstring of string Rendition.t
    | Tok_istring of string Rendition.t
    | Tok_fstring_lditto
    | Tok_fstring_interpolated of string Rendition.t
    | Tok_fstring_pct
    | Tok_fstring_pad of codepoint Rendition.t
    | Tok_fstring_just of Fmt.just
    | Tok_fstring_sign of Fmt.sign
    | Tok_fstring_alt
    | Tok_fstring_zpad
    | Tok_fstring_width_star
    | Tok_fstring_width of uns Rendition.t
    | Tok_fstring_pmode of Fmt.pmode
    | Tok_fstring_precision_star
    | Tok_fstring_precision of uns Rendition.t
    | Tok_fstring_radix of Radix.t
    | Tok_fstring_notation of Fmt.notation
    | Tok_fstring_pretty
    | Tok_fstring_fmt of fmt Rendition.t
    | Tok_fstring_sep of string Rendition.t
    | Tok_fstring_label of string
    | Tok_fstring_lparen_caret
    | Tok_fstring_caret_rparen
    | Tok_fstring_rditto
    | Tok_r32 of real Rendition.t
    | Tok_r64 of real Rendition.t
    | Tok_u8 of u8 Rendition.t
    | Tok_i8 of i8 Rendition.t
    | Tok_u16 of u16 Rendition.t
    | Tok_i16 of i16 Rendition.t
    | Tok_u32 of u32 Rendition.t
    | Tok_i32 of i32 Rendition.t
    | Tok_u64 of u64 Rendition.t
    | Tok_i64 of i64 Rendition.t
    | Tok_u128 of u128 Rendition.t
    | Tok_i128 of i128 Rendition.t
    | Tok_u256 of u256 Rendition.t
    | Tok_i256 of i256 Rendition.t
    | Tok_u512 of u512 Rendition.t
    | Tok_i512 of i512 Rendition.t
    | Tok_nat of Nat.t Rendition.t
    | Tok_zint of Zint.t Rendition.t
    | Tok_end_of_input
    | Tok_misaligned
    | Tok_error of Rendition.Malformation.t list

  val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

  val malformations: t -> Rendition.Malformation.t list
  (** [malformations t] returns a list of malformations associated with [t], or an empty list if
      there are no malformations. This function can be used on any token variant, even if no
      malformations are possible. *)
end

(** Concrete tokens augment abstract tokens with source locations. *)
module ConcreteToken : sig
  type t = {
    atok: AbstractToken.t;
    source: Source.Slice.t;
  }

  val atok: t -> AbstractToken.t
  val source: t -> Source.Slice.t

  val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
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

val next: t -> t * ConcreteToken.t
(** [next t] scans the next token past the tokens scanned by [t]'s predecessor state(s) and returns
    the scanner's successor state along with a token. If [t] is at the end of input, there is no
    successor state, and [t, EndOfInput] is returned. *)
