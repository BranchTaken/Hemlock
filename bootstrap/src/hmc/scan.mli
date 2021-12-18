(** Scanner.

    The following naming convention is used for special codepoints which show up in the language
    syntax. Token names for other codepoints are e.g. Tok_a and Tok_A; concatenations are e.g.
    Tok_a_A. The special codepoints are referred to similarly, e.g. Tok_nl and Tok_lbrack_bar.

    - '\n' : nl
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

module Source : sig
  type t

  include FormattableIntf.SMono with type t := t
  val pp_loc: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

  val path: t -> string option

  module Cursor : sig
    include CursorIntf.SMonoIndex
      with type container := t
      with type elm := codepoint

    val pos: t -> Text.Pos.t
  end

  module Slice : sig
    include SliceIntf.SMonoIndex
      with type container := t
      with type cursor := Cursor.t
      with type elm := codepoint
  end

  val line_context: t -> Slice.t * t
  (** [line_context t] creates an expanded source which contains the entirety of the line(s) on
      which [t] resides, as well as a slice of the expanded source which corresponds to [t]. *)
end

module AbstractToken : sig
  module Rendition : sig
    module Malformation : sig
      type t

      include FormattableIntf.SMono with type t := t

      val init: path:string option -> line_bias:sint -> col_bias:sint -> base:Text.Cursor.t
        -> past:Text.Cursor.t -> description:string -> t
      val source: t -> Source.t
      val description: t -> string
    end
    type 'a t =
      | Constant of 'a
      | Malformed of Malformation.t list

    include FormattableIntf.SPoly with type 'a t := 'a t
  end
  type source_directive = {
    path: string option;
    line: uns option;
    col: uns option;
  }
  type t =
    (* Keywords. *)
    | Tok_and
    | Tok_also
    | Tok_as
    | Tok_assert
    | Tok_conceal
    | Tok_effect
    | Tok_else
    | Tok_expose
    | Tok_external
    | Tok_false
    | Tok_fun
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
    | Tok_val
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
    | Tok_semi
    | Tok_semi_semi
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
    | Tok_larray
    | Tok_rarray
    | Tok_lmodule
    | Tok_rmodule
    | Tok_bslash
    | Tok_tick
    | Tok_caret
    | Tok_amp
    | Tok_xmark
    | Tok_arrow
    | Tok_carrow

    | Tok_source_directive of source_directive Rendition.t
    | Tok_indent of unit Rendition.t
    | Tok_line_delim
    | Tok_dedent of unit Rendition.t
    | Tok_whitespace
    | Tok_hash_comment
    | Tok_paren_comment of unit Rendition.t
    | Tok_uscore
    | Tok_uident of string Rendition.t
    | Tok_cident of string
    | Tok_codepoint of codepoint Rendition.t
    | Tok_istring_lditto
    | Tok_isubstring of string Rendition.t
    | Tok_istring_pct
    | Tok_istring_lparen_caret
    | Tok_istring_caret_rparen
    | Tok_istring_rditto
    | Tok_rstring of string Rendition.t
    | Tok_bstring of string Rendition.t
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
    | Tok_end_of_input
    | Tok_misaligned
    | Tok_error

  val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
end

module ConcreteToken : sig
  type t

  val atoken: t -> AbstractToken.t
  val source: t -> Source.t

  val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
end

type t

val init: Text.t -> t
(** [init text] initializes scanner to scan [text]. *)

val text: t -> Text.t
(** [text t] returns the source text for [t]. *)

val next: t -> t * ConcreteToken.t
(** [next t] scans the next token past the tokens scanned by [t]'s predecessor state(s) and returns
    the scanner's successor state along with a token. If [t] is at the end of input, there is no
    successor state, and [t, EndOfInput] is returned. *)
