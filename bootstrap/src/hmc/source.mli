(** Wrapper around Text that supports source directives via "biasing". *)

open Basis
open Basis.Rudiments

type t

include FormattableIntf.SMono with type t := t

val init: Text.t -> t
(** [init text] initializes source without bias. *)

val bias: path:Path.t option -> line_bias:sint -> col_bias:sint -> t -> t
(** [bias ~path ~line_bias ~col_bias t] creates a biased version of [t]. The result is independent
    of the bias of [t], if any, i.e. [bias ... t] produces the same result as [bias ... (unbias t)].
*)

val unbias: t -> t
(** [unbias t] creates an unbiased version of [t]. *)

val text: t -> Text.t
(** [text t] returns the text with which [t] was created. *)

val path: t -> Path.t option
(** [path t] returns the current (potentially biased) path. *)

val line_bias: t -> sint
(** [line_bias t] returns the current line bias. *)

val col_bias: t -> sint
(** [col_bias t] returns the current column bias. *)

module Cursor : sig
  include CursorIntf.SMonoIndex
    with type container = t
    with type elm := codepoint

  include FormattableIntf.SMono with type t := t

  val pos: t -> Text.Pos.t
  (** [pos t] returns the (potentially biased) cursor position. *)

  val bias: container -> t -> t
  (** [bias source t] creates a biased version of [t] at the same text position, but with [source]'s
      bias. When viewing leftward (e.g. [lget]) or moving leftwards (e.g. [pred]) past the location
      of [t] the operation is based on [t] in order to recover its bias. This makes it possible to
      move leftwards and always recover the bias of predecessor cursors. *)

  val debias: t -> t
  (** [debias t] creates a version of [t] with prior bias (if any). *)

  val unbias: t -> t
  (** [unbias t] creates a version of [t] with no bias. *)

  val bias_prior: t -> t
  (** [bias_prior t] returns the cursor which was specified to [bias source t]. Note that [t] may be
      rightward of [bias_prior t]. *)

  val text_cursor: t -> Text.Cursor.t
  (** [text_cursor t] returns the text cursor corresponding to [t]. *)

  val next_opt: t -> (codepoint * t) option
  (** [next_opt t] returns [Some (codepoint, t')] if [t] is not at the text's tail, [None]
      otherwise. Source data are lazily streamed as needed. *)

  val rvalid: t -> bool
  (** [rvalid t] returns whether the byte sequence to the right of the cursor is valid UTF-8. Note
      that [rget t] returns '�' if the byte sequence is invalid, but valid UTF-8 may also directly
      encode '�'. *)
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

  val line_context: ?lookahead:Cursor.t -> t -> t list
  (** [line_context ~lookahead t] returns an expanded context that encompasses the entirety of the
      line(s) on which [t] resides. The result is a non-empty list of slices, where each slice has a
      single source. The result typically comprises a single slice, but due to the potential for [t]
      be split into multiple slices if it crosses source transitions, there is not necessarily a
      direct correspondence between [t] and one of the resulting slices. If [lookahead] is
      specified, it must satisfy [Cursor.((past t) <= lookahead], and it is used as the starting
      position for the result's right bound; otherwise [past t] is used as the starting point. If
      [lookahead] is not specified or it precedes the end of the line on which [t] ends, all
      codepoints past the starting position of the right bound search are considered to have
      unbiased source. Ideally the caller will specify [lookahead] by extracting a cursor from a
      token far enough to the right of [t] to encompass the remainder of the line on which [t] ends
      so that the search for the right bound is a leftward search, but during scanning there may not
      yet be sufficient context to do so. *)

  val to_string: t -> string
  (** [to_string t] returns a string representation of [t]. *)
end
