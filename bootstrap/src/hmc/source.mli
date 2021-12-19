(** Wrapper around Text that supports source directives via "biasing". *)

open Basis
open Basis.Rudiments

type t

include FormattableIntf.SMono with type t := t

val init: Text.t -> t
(* [init text] initializes source without bias. *)

val bias: path:string option -> line_bias:sint -> col_bias:sint -> t -> t
(* [bias ~path ~line_bias ~col_bias t] creates a biased version of [t]. *)

val unbias: t -> t
(* [unbias t] creates an unbiased version of [t]. *)

val text: t -> Text.t
(* [text t] returns the text with which [t] was created. *)

val path: t -> string option
(* [path t] returns the current (potentially biased) path. *)

val line_bias: t -> sint
(* [line_bias t] returns the current line bias. *)

val col_bias: t -> sint
(* [col_bias t] returns the current column bias. *)

module Cursor : sig
  include CursorIntf.SMonoIndex
    with type container = t
    with type elm := codepoint

  include FormattableIntf.SMono with type t := t

  val pos: t -> Text.Pos.t
  (* [pos t] returns the (potentially biased) cursor position. *)

  val bias: container -> t -> t -> t
  (** [bias source prior t] creates a biased version of [t] with [source]'s bias, and [prior] as a
      cursor at the same text position, but with prior bias. When viewing (e.g. [lget]) or moving
      leftwards (e.g. [pred]) past the location of [prior] the operation is based on [prior] in
      order to recover its bias. This makes it possible to move leftwards and always recover the
      bias of predecessor cursors. *)

  val debias: t -> t
  (* [debias t] creates a version of [t] with prior bias (if any). *)

  val unbias: t -> t
  (* [unbias t] creates a version of [t] with no bias. *)

  val bias_prior: t -> t
  (* [bias_prior t] returns the [prior] cursor which was specified in [bias source prior t]. Note
   * that [t] may be rightward of [bias_prior t]. *)

  val text_cursor: t -> Text.Cursor.t
  (* [text_cursor t] returns the text cursor corresponding to [t]. *)

  val next_opt: t -> (codepoint * t) option
  (** [next_opt t] returns [Some (codepoint, t')] if [t] is not at the text's tail, [None]
      otherwise. Source data are lazily streamed as needed. *)

  val nextv_opt: t -> (codepoint * bool * t) option
  (** [nextv_opt t] returns [Some (codepoint, valid, t')] if [t] is not at the text's tail, [None]
      otherwise. [codepoint] is valid unless it is '�' and the result of invalid byte encoding.
      Source data are lazily streamed as needed. *)
end

module Slice : sig
  include SliceIntf.SMonoIter
    with type container := t
    with type cursor := Cursor.t
    with type elm := codepoint

  include FormattableIntf.SMono with type t := t

  val cmp: t -> t -> Cmp.t
  (** [cmp t0 t1] compares [base] of [t0] and [t1], and falls back to comparing [past] of [t0] and
      [t1] if the bases * are equal. *)

  val of_cursors: base:Cursor.t -> past:Cursor.t -> t
  (** [of_cursors ~base ~past] creates a slice with contents \[[base .. past)]. *)

  val line_context: t -> t
  (** [line_context t] creates an expanded slice which contains the entirety of the line(s) on
      which [t] resides. If the slice spans multiple sources (as specified via source directives)
      and the beginning line context extends through a source boundary, the base source is reported
      as being the unbiased source (i.e. source directives are ignored). Similarly, if the ending
      line context extends past the input slice or the base source is reported as unbiased, the past
      source is unconditionally reported as being the unbiased source. This behavior avoids false
      source origin claims. *)

  val to_string: t -> string
  (** [to_string t] returns a string representation of [t]. *)
end
