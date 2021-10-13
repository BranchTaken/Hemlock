(** Multi-line text, as might be stored in a UTF-8-encoded file. Any encoding errors present in the
    input used to construct the text are automatically converted to '�', and cursor indices are
    based on the replaced representation, thus maintaining the invariant that the text exclusively
    comprises codepoints.

    The text is presented as a linear sequence of codepoints which is lazily streamed as needed to
    satisfy cursor operations, length queries, etc. The lazy suspension has no impact on the API,
    but the intent is to support linear forward scanning such that if the tail of the stream is
    never needed, the application does not incur the cost of complete text initialization. *)

open Rudiments

type t
(** Text. *)

val of_bytes_stream: ?path:string -> ?tabwidth:uns -> Bytes.Slice.t Stream.t -> t
(** [of_string_stream ~path ~tabwidth stream] returns a text which streams from [stream].
    [~tabwidth] defaults to 8. *)

val of_string_slice: ?path:string -> ?tabwidth:uns -> String.Slice.t -> t
(** [of_string_slice ~path slice] returns an eagerly initialized text. [~tabwidth] defaults to 8. *)

val path: t -> string option
(** [path t] returns the optional path associated with the text. *)

val tabwidth: t -> uns
(** [tabwidth t] returns the column multiple for tab stops. *)

(** Position within a text. The associated text is intentionally not referenced, lest values retain
    references to arbitrary incremental texts. *)
module Pos: sig
  type t
  (** Position type. *)

  include FormattableIntf.SMono with type t := t

  val init: line:uns -> col:uns -> t
  (** Initialize position. *)

  val line: t -> uns
  (** [line t] returns the position's line, where line numbering starts at 1. *)

  val col: t -> uns
  (** [col t] returns the position's column, where column numbering starts at 0. *)

  include CmpableIntf.SMono with type t := t
end

(** Text cursor which tracks position (line and column) in addition to codepoint index. Seeks
    perform work proportional to seek distance, and all cursors must derive from [hd], which means
    that it is important to retain cursors to positions of later interest rather than repeatedly
    seeking. [tl] forces the source stream and seeks from [hd], and should therefore be avoided if
    efficiency is a concern. Prefer [rget_opt]/[next_opt] over their halting [rget]/[next]
    counterparts to avoid the need for [tl] calls. *)
module Cursor : sig
  include CursorIntf.SMonoIndex
    with type container := t
    with type elm := codepoint

  val rget_opt: t -> codepoint option
  (** [rget_opt t] returns [Some codepoint] if [t] is not at the text's tail, [None] otherwise.
      Source data are lazily streamed as needed. *)

  val rgetv_opt: t -> (codepoint * bool) option
  (** [rgetv_opt t] returns [Some (codepoint, valid)] if [t] is not at the text's tail, [None]
      otherwise. [codepoint] is valid unless it is '�' and the result of invalid byte encoding.
      Source data are lazily streamed as needed. *)

  val next_opt: t -> (codepoint * t) option
  (** [next_opt t] returns [Some (codepoint, t')] if [t] is not at the text's tail, [None]
      otherwise. Source data are lazily streamed as needed. *)

  val nextv_opt: t -> (codepoint * bool * t) option
  (** [nextv_opt t] returns [Some (codepoint, valid, t')] if [t] is not at the text's tail, [None]
      otherwise. [codepoint] is valid unless it is '�' and the result of invalid byte encoding.
      Source data are lazily streamed as needed. *)

  val nextv: t -> codepoint * bool * t
  (** [nextv t] returns [codepoint, valid, t'] if [t] is not at the text's tail, halts otherwise.
      [codepoint] is valid unless it is '�' and the result of invalid byte encoding. Source data are
      lazily streamed as needed. *)

  val rvalid: t -> bool
  (** [rvalid t] returns whether the byte sequence to the right of the cursor is valid UTF-8. Note
      that [rget t] returns '�' if the byte sequence is invalid, but valid UTF-8 may also directly
      encode '�'. *)

  val prevv: t -> codepoint * bool * t
  (** [prevv t] returns [codepoint, valid, t'] if [t] is not at the text's head, halts otherwise.
      [codepoint] is valid unless it is '�' and the result of invalid byte encoding. *)

  val lvalid: t -> bool
  (** [lvalid t] returns whether the byte sequence to the left of the cursor is valid UTF-8. Note
      that [lget t] returns '�' if the byte sequence is invalid, but UTF-8 may also directly encode
      '�'. *)

  val pos: t -> Pos.t
  (** [pos t] returns the position of [t]. Note that if [t] derives from having moved backward over
      a ['\n'] or ['\t'] as recently as having moved from column 0, the column number must be
      calculated. *)
end

(** Text slice. *)
module Slice : sig
  include SliceIntf.SMonoIter
    with type container := t
    with type cursor := Cursor.t
    with type elm := codepoint

  val to_string: t -> string
  (** [to_string t] returns a string representation of [t]. *)
end
