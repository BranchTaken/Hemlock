(** Abstract filesystem path manipulation. *)

(** Path segment. *)
module Segment: sig
  type t
  (** Path segment type. *)

  include FormattableIntf.SMono with type t := t

  val to_string: t -> string option
  (** [to_string t] converts [t] to a string, or returns [None] if the segment cannot be represented
      as UTF-8. *)

  val to_string_replace: t -> string
  (** [to_string t] converts [t] to a string, with invalid UTF-8 converted to one or more '�'
      replacement codepoints. *)

  val to_string_hlt: t -> string
  (** [to_string_hlt t] converts [t] to a string, or halts if the segment cannot be represented as
      UTF-8. *)

  val to_bytes: t -> Bytes.Slice.t
  (** [to_bytes t] converts [t] to a bytes slice. *)

  val is_empty: t -> bool
  (** [is_current t] returns true if the segment is empty (zero length string representation). *)

  val is_current: t -> bool
  (** [is_current t] returns true if the segment is [.]. *)

  val is_parent: t -> bool
  (** [is_current t] returns true if the segment is [..]. *)

  val split: t -> t list
  (** [split t] splits the segment into its stem and [.]-initiated suffixes, if any. *)

  val stem: t -> t
  (** [stem t] returns [List.hd (split t)]. *)

  val suffixes: t -> t list
  (** [suffixes t] returns [List.tl (split t)]. *)

  val suffix: t -> t
  (** [suffix t] returns the last [.]-initiated suffix of [t], or an empty segment if [t] has no
      suffixes. *)

  val join: t list -> t
  (** [join split] joins [split] into a single segment. Halt on any multi-segment input which
      contains a current or parent segment. *)
end

type t
(** Path type. *)

include FormattableIntf.SMono with type t := t

val of_string: string -> t
(** [of_string s] creates a path by splitting [s] at [/] segment separators. *)

val of_bytes: Bytes.Slice.t -> t
(** [of_bytes bslice] creates a path by splitting [bslice] at [/] segment separators. *)

val of_segment: Segment.t -> t
(** [of_segment segment] creates a path from a single segment. *)

val is_abs: t -> bool
(** [is_abs t] returns true if [t] is an absolute path. *)

val is_empty: t -> bool
(** [is_empty t] returns true if [t] is an empty path. *)

val dirname: t -> t
(** [dirname t] returns the path leading to the last segment of [t], if any. *)

val basename: t -> Segment.t option
(** [basename t] returns the last segment of [t], or [None] if an empty path. *)

val split: t -> t * Segment.t option
(** [split t] is equivalent to [(dirname t), (basename t)]. *)

val normalize: t -> t
(** [normalize t] performs the following path transformations:

    - [///a] -> [/a]: Reduce three or more leading separators to one separator (absolute path).
    - [a//b] -> [a/b]: Remove internal empty segments.
    - [a/] -> [a]: Remove trailing empty segments.
    - [./a/./b/.] -> [a/b]: Remove [.] segments.
    - [a/../b] -> [b]: Collapse parent directory segments with parents. This may change how the
      path resolves if the parent is a symbolic link. *)

val join: t list -> t
(** [join paths] joins [paths] into a single path. NB: Absolute paths start with an empty segment,
    but only the first non-empty path in a join affects whether the result is an absolute path.
    Leading empty segments in the other input path(s) may be interpreted in a variety of potentially
    surprising contextually dependent ways. *)

val to_string: t -> string option
(** [to_string t] converts [t] to a string, or returns [None] if the path cannot be represented as
    UTF-8. *)

val to_string_replace: t -> string
(** [to_string t] converts [t] to a string, with invalid UTF-8 converted to one or more '�'
    replacement codepoints. *)

val to_string_hlt: t -> string
(** [to_string_hlt t] converts [t] to a string, or halts if the path cannot be represented as UTF-8.
*)

val to_bytes: t -> Bytes.Slice.t
(** [to_bytes t] converts [t] to a bytes slice. *)
