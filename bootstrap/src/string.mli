(** A {!type:string} is an immutable {!type:byte} sequence that is restricted to
    contain a well-formed concatenation of UTF-8-encoded {!type:codepoint}
    values.  Indexed {!type:byte} access is O(1), but indexed {!type:codepoint}
    access is O(n).  The {!String.Cursor} module provides {!type:codepoint}
    access without tracking {!type:codepoint} index; for atypical cases that
    actually require {!type:codepoint} indexing, use the {!String.Cursori}
    module instead.

    The {!String.Slice} module provides an API similar to that of the {!String}
    module, but it applies to a narrowed view -- {!type:slice} -- of the
    containing {!type:string}.  Slices avoid copying, which makes them both
    convenient and efficient.
*)

open Rudiments

type t = string

include Identifiable_intf.S with type t := t

(** Cursor that supports arbitrary access to codepoints, given byte index.  The
    codepoint index is not tracked. *)
module Cursor : sig
  type outer = t

  type t
  include Cursor_intf.S_mono with type container := outer
                              and type elm := codepoint
                              and type t := t

  val index: t -> uint [@@ocaml.deprecated "Use bindex instead"]
  (** @deprecated Use {!bindex} instead.
      @raise halt Not implemented. *)

  val bindex: t -> uint
  (** @return Current {!type:byte} index. *)

  val cindex: t -> uint [@@ocaml.deprecated "Do not use; O(n)"]
  (** @deprecated Use {!Cursori.cindex} instead.
      @raise halt Not implemented. *)

  val at: outer -> bindex:uint -> t
  (** @param bindex {!type:byte} index.
      @return {!type:cursor} at [bindex].
      @raise halt Not at {!type:codepoint} boundary.
  *)

  val near: outer -> bindex:uint -> t
  (** @param bindex {!type:byte} index.
      @return {!type:cursor} at or before [bindex]. *)
end
type cursor = Cursor.t

(** Cursor that tracks codepoint index.  Arbitrary codepoint access via
    Cursori.at requires linear scanning, unlike Cursor.at .  Prefer Cursor
    unless {!type:codepoint} index is needed. *)
module Cursori : sig
  type outer = t

  type t
  include Cursor_intf.S_mono with type container := outer
                              and type elm := codepoint
                              and type t := t
  val index: t -> uint [@@ocaml.deprecated "Use [bc]index instead"]
  (** @deprecated Use {!bindex} or {!cindex} instead.
      @raise halt Not implemented. *)

  val bindex: t -> uint
  (** @return Current {!type:byte} index. *)

  val cindex: t -> uint
  (** @return Current {!type:codepoint} index. *)

  val cursor: t -> cursor
  (** @return Encapsulated {!type:cursor}. *)

  val at: outer -> cindex:uint -> t
  (** @param cindex {!type:codepoint} index.
      @return {!type:cursori} at [cindex].
  *)
end
type cursori = Cursori.t

type slice
module Slice : sig
  type outer = t
  type t = slice
  include Cmpable_intf.S with type t := t

  val of_cursors: base:cursor -> past:cursor -> t
  val to_cursors: t -> cursor * cursor
  val string: t -> outer
  val base: t -> cursor
  val past: t -> cursor

  val of_string: outer -> t
  val to_string: t -> outer

  val base_seek: t -> int -> t
  val base_succ: t -> t
  val base_pred: t -> t

  val past_seek: t -> int -> t
  val past_succ: t -> t
  val past_pred: t -> t

  val blength: t -> uint
  val clength: t -> uint
  val get: t -> uint -> byte

  val init: ?blength:uint -> uint -> f:(uint -> codepoint) -> t

  val of_codepoint: codepoint -> t

  val of_list: ?blength:uint -> ?clength:uint -> codepoint list -> t
  val of_list_rev: ?blength:uint -> ?clength:uint -> codepoint list
    -> t
  (* In Container_intf.S_mono: val to_list: t -> codepoint list *)
  (* In Container_intf.S_mono: val to_list_rev: t -> codepoint list *)

  val of_array: codepoint array -> t
  (* In Container_intf.S_mono: val to_array: t -> codepoint array *)

  include Container_intf.S_mono with type t := t and type elm := codepoint
  val length: t -> uint [@@ocaml.deprecated "Use blength instead"]

  val map: t -> f:(codepoint -> codepoint) -> t
  val mapi: t -> f:(uint -> codepoint -> codepoint) -> t
  val tr: target:codepoint -> replacement:codepoint -> t -> t
  val filter: t -> f:(codepoint -> bool) -> t
  val concat: ?sep:t -> t list -> t
  val concat_rev: ?sep:t -> t list -> t
  val concat_map: ?sep:t -> t -> f:(codepoint -> t) -> t
  val escaped: t -> t
  val rev: t -> t

  val lfind: t -> codepoint -> cursor option
  val lfind_hlt: t -> codepoint -> cursor
  val contains: t -> codepoint -> bool

  val rfind: t -> codepoint -> cursor option
  val rfind_hlt: t -> codepoint -> cursor

  module Pattern : sig
    type outer = t
    type t
    val create: outer -> t
    val find: t -> in_:outer -> cursor option
    val find_hlt: t -> in_:outer -> cursor
    val find_all: t -> may_overlap:bool -> in_:outer -> cursor list
    val replace_first: t -> in_:outer -> with_:outer -> outer
    val replace_all: t -> in_:outer -> with_:outer -> outer
  end

  val is_prefix: t -> prefix:t -> bool
  val is_suffix: t -> suffix:t -> bool

  val prefix: t -> uint -> t
  val suffix: t -> uint -> t

  val chop_prefix: t -> prefix:t -> t option
  val chop_prefix_hlt: t -> prefix:t -> t

  val chop_suffix: t -> suffix:t -> t option
  val chop_suffix_hlt: t -> suffix:t -> t

  val lstrip: ?drop:(codepoint -> bool) -> t -> t
  val rstrip: ?drop:(codepoint -> bool) -> t -> t
  val strip: ?drop:(codepoint -> bool) -> t -> t

  val split_fold_until: t -> init:'accum -> on:(codepoint -> bool)
    -> f:('accum -> slice -> 'accum * bool) -> 'accum
  val split_fold_right_until: t -> init:'accum -> on:(codepoint -> bool)
    -> f:(slice -> 'accum -> 'accum * bool) -> 'accum
  val split_fold: t -> init:'accum -> on:(codepoint -> bool)
    -> f:('accum -> slice -> 'accum) -> 'accum
  val split_fold_right: t -> init:'accum -> on:(codepoint -> bool)
    -> f:(slice -> 'accum -> 'accum) -> 'accum
  val lines_fold: t -> init:'accum -> f:('accum -> slice -> 'accum) -> 'accum
  val lines_fold_right: t -> init:'accum -> f:(slice -> 'accum -> 'accum)
    -> 'accum

  val lsplit2: t -> on:codepoint -> (t * t) option
  val lsplit2_hlt: t -> on:codepoint -> t * t

  val rsplit2: t -> on:codepoint -> (t * t) option
  val rsplit2_hlt: t -> on:codepoint -> t * t

  module O : sig
    type nonrec t = t

    include Cmpable_intf.S_infix with type t := t
  end
end

module Seq : sig
  type outer = t
  module type S = sig
    type t
    val to_string: t -> outer
  end

  (** Efficiently convert a codepoint sequence with known blength to a string.
      The length function returns blength of the remaining sequence; the next
      function returns the next codepoint which is converted to bytes in
      to_string. *)
  module Codepoint : sig
    module Make (T : Seq_intf.I_mono_def with type elm := codepoint) :
      S with type t := T.t
    module Make_rev (T : Seq_intf.I_mono_def with type elm := codepoint) :
      S with type t := T.t
  end

  (** Efficiently convert a string slice sequence with known blength to a
      string.  The length function returns blength of the remaining sequence;
      the next function returns (base, past) cursors for the next string slice
      which is copied into to_string. *)
  module Slice : sig
    module Make (T : Seq_intf.I_mono_def with type elm := slice) :
      S with type t := T.t
    module Make_rev (T : Seq_intf.I_mono_def with type elm := slice) :
      S with type t := T.t
  end

  (** Efficiently convert a string sequence with known blength to a string.  The
      length function returns blength of the remaining sequence; the next
      function returns the next string which is copied into to_string. *)
  module String : sig
    module Make (T : Seq_intf.I_mono_def with type elm := string) :
      S with type t := T.t
    module Make_rev (T : Seq_intf.I_mono_def with type elm := string) :
      S with type t := T.t
  end
end

val blength: t -> uint
(** Byte length. *)

val clength: t -> uint
(** Codepoint length. *)

val get: t -> uint -> byte
(** Get byte at index. *)

val init: ?blength:uint -> uint -> f:(uint -> codepoint) -> t

val of_codepoint: codepoint -> t
(** Create a string from a codepoint. *)

val of_list: ?blength:uint -> ?clength:uint -> codepoint list -> t
val of_list_rev: ?blength:uint -> ?clength:uint -> codepoint list
  -> t
(* In Container_intf.S_mono: val to_list: t -> codepoint list *)
(* In Container_intf.S_mono: val to_list_rev: t -> codepoint list *)

val of_array: codepoint array -> t
(* In Container_intf.S_mono: val to_array: t -> codepoint array *)

include Container_intf.S_mono with type t := t and type elm := codepoint
val length: t -> uint [@@ocaml.deprecated "Use [bc]length instead"]

val map: t -> f:(codepoint -> codepoint) -> t
val mapi: t -> f:(uint -> codepoint -> codepoint) -> t
val tr: target:codepoint -> replacement:codepoint -> t -> t
val filter: t -> f:(codepoint -> bool) -> t
val concat: ?sep:t -> t list -> t
val concat_rev: ?sep:t -> t list -> t
val concat_map: ?sep:t -> t -> f:(codepoint -> t) -> t
val escaped: t -> t
val rev: t -> t
val ( ^ ): t -> t -> t

val lfind: ?base:cursor -> ?past:cursor -> t -> codepoint -> cursor option
val lfind_hlt: ?base:cursor -> ?past:cursor -> t -> codepoint -> cursor
val contains: ?base:cursor -> ?past:cursor -> t -> codepoint -> bool

val rfind: ?base:cursor -> ?past:cursor -> t -> codepoint -> cursor option
val rfind_hlt: ?base:cursor -> ?past:cursor -> t -> codepoint -> cursor

val substr_find: ?base:cursor -> t -> pattern:t -> cursor option
val substr_find_hlt: ?base:cursor -> t -> pattern:t -> cursor
val substr_find_all: t -> may_overlap:bool -> pattern:t -> cursor list
val substr_replace_first: ?base:cursor -> t -> pattern:t -> with_:t -> t
val substr_replace_all: t -> pattern:t -> with_:t -> t

val is_prefix: t -> prefix:t -> bool
val is_suffix: t -> suffix:t -> bool

val pare: base:cursor -> past:cursor -> t

val prefix: t -> uint -> t
val suffix: t -> uint -> t

val chop_prefix: t -> prefix:t -> t option
val chop_prefix_hlt: t -> prefix:t -> t

val chop_suffix: t -> suffix:t -> t option
val chop_suffix_hlt: t -> suffix:t -> t

val lstrip: ?drop:(codepoint -> bool) -> t -> t
val rstrip: ?drop:(codepoint -> bool) -> t -> t
val strip: ?drop:(codepoint -> bool) -> t -> t

val split: t -> f:(codepoint -> bool) -> t list
val split_rev: t -> f:(codepoint -> bool) -> t list
val split_lines: t -> t list
val split_lines_rev: t -> t list

val lsplit2: t -> on:codepoint -> (t * t) option
val lsplit2_hlt: t -> on:codepoint -> t * t

val rsplit2: t -> on:codepoint -> (t * t) option
val rsplit2_hlt: t -> on:codepoint -> t * t

module O : sig
  type nonrec t = t

  include Cmpable_intf.S_infix with type t := t
end
