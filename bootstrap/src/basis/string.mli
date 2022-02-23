(** UTF-8-encoded immutable {!type:string}.

    A {!type:string} is an immutable {!type:byte} sequence that is restricted to contain a
    well-formed concatenation of UTF-8-encoded {!type:codepoint} values. The {!String.B} module
    provides O(1) {!type:byte}-indexed functionality. The {!String.C} module provides O(n)
    {!type:codepoint} iteration functionality. Note that {!type:codepoint} cursor index is
    intentionally not tracked for performance reasons, but in practice this does not cause problems
    for typical string manipulation. Note that {!String.C.Slice} provides indexing during iteration,
    and {!type:codepoint} length is available via {!String.C.length}/{!String.C.Slice.length}, but
    at O(n) time complexity.

    The {!String.C.Slice} module provides an API similar to that of the {!String} module, but it
    applies to a narrowed view -- {!type:String.Slice.t} -- of the contained {!type:string}. Slices
    avoid copying, which makes them both convenient and efficient. *)

open Rudiments0

type t = string

include IdentifiableIntf.S with type t := t
include StringableIntf.S with type t := t

(** Byte-addressed string manipulation. *)
module B : sig
  val length: t -> uns
  (** Byte length. *)

  val get: uns -> t -> byte
  (** Get byte at index. *)

  (** Cursor that supports O(1) arbitrary access to bytes. *)
  module Cursor : sig
    type container = t
    type elm = byte
    type t
    include IdentifiableIntf.S with type t := t
    include CursorIntf.SMonoIndex
      with type container := container
      with type elm := elm
      with type t := t

    val string: t -> container
    (** Return string associated with cursor. *)

    val init: uns -> container -> t
    (** [init index string] returns a cursor at the specified byte [index] of [string]. *)

    val at: uns -> container -> t
    (** [at index s] return a {!type:Cursor.t} at [index], or halts if not at a {!type:codepoint}
        boundary. *)

    val near: uns -> container -> t
    (** [near index s] returns a {!type:Cursor.t} at or before [index]. *)
  end

  module Slice : sig
    include SliceIntf.SMonoIndex
      with type container := t
      with type cursor := Cursor.t
      with type elm := byte
    include IdentifiableIntf.S with type t := t
    include ContainerIntf.SMonoIndex with type t := t with type elm := byte
    include ContainerIntf.SMonoMem with type t := t with type elm := byte

    val get: uns -> t -> byte
    (** Get byte at index. *)
  end
end

(** Codepoint-addressed string manipulation. *)
module C : sig
  val length: t -> uns
  (** Codepoint length. *)

  (** Cursor that supports O(1) codepoint iteration. The codepoint index is not tracked. *)
  module Cursor : sig
    type container = t
    type elm = codepoint
    type t
    include IdentifiableIntf.S with type t := t
    include CursorIntf.SMonoIter
      with type container := container
      with type elm := elm
      with type t := t

    val bindex: t -> uns
    (** [bindex t] return the byte index of [t]. Equivalent to [B.Cursor.index (to_bcursor t)]. *)

    val string: t -> container
    (** Return string associated with cursor. *)

    val at: B.Cursor.t -> t
    (** [at bcursor] returns a cursor at [bcursor], or halts if not at a {!type:codepoint} boundary.
    *)

    val near: B.Cursor.t -> t
    (** [near bcursor] returns a cursor at the nearest {!type:codepoint} boundary at or before
        [bcursor]. *)

    val to_bcursor: t -> B.Cursor.t
    (** [to_bcursor t] returns a byte cursor at the same location as [t]. *)

    val seek: sint -> t -> t
    (** [seek i t] returns a cursor at offset [i] from [t]. *)
  end

  module Slice : sig
    include SliceIntf.SMonoIter
      with type container := t
      with type cursor := Cursor.t
      with type elm := codepoint
    include IdentifiableIntf.S with type t := t

    val blength: t -> uns
    (** [blength t] returns the number of bytes enclosed by [t]. Equivalent to [B.Slice.length
        (to_bslice t)]. *)

    val to_bslice: t -> B.Slice.t
    (** [to_bslice t] returns a byte slice for the same range as [t]. *)

    val of_cursors: base:Cursor.t -> past:Cursor.t -> t
    (** [of_cursors ~base ~past] creates a slice enclosing the codepoints in \[[base .. past)]. *)

    val of_string: string -> t
    (** [of_string s] creates a slice enclosing [s]. *)

    val to_string: t -> string
    (** Return a string with contents equivalent to those of the slice. *)

    val init: ?blength:uns -> range -> f:(uns -> codepoint) -> t
    (** [init ~blength crange ~f:(fun i -> ...)] creates a slice of given byte length [~blength] and
        codepoint length [Range.length crange] using [~f] to map range elements to codepoints.
        [blength] must be accurate if specified. *)

    val of_codepoint: codepoint -> t
    (** Create a slice containing a single codepoint. *)

    val of_list: ?blength:uns -> codepoint list -> t
    (** [of_list ~blength codepoints] creates a slice of given byte length containing the ordered
        [codepoints]. [blength] must be accurate if specified. *)

    val of_list_rev: ?blength:uns -> codepoint list -> t
    (** [of_list_rev ~blength codepoints] creates a slice of given byte length containing the
        reverse-ordered [codepoints]. [blength] must be accurate if specified. *)

    val of_array: ?blength:uns -> codepoint array -> t
    (** [of_array ~blength codepoints] creates a slice of given byte length containing the ordered
        [codepoints]. [blength] must be accurate if specified. *)

    include ContainerIntf.SMonoIndex with type t := t with type elm := codepoint
    include ContainerIntf.SMonoMem with type t := t with type elm := codepoint

    val map: f:(codepoint -> codepoint) -> t -> t
    (** [map ~f t] creates a slice with codepoints mapped from [t]'s codepoints. *)

    val mapi: f:(uns -> codepoint -> codepoint) -> t -> t
    (** [map ~f t] creates a slice with codepoints mapped from [t]'s codepoints. Codepoint index
        within [t] is provided to [f]. *)

    val tr: target:codepoint -> replacement:codepoint -> t -> t
    (** [tr ~target ~replacement t] creates a slice with select codepoints translated from [t]'s
        codepoints. [target] is translated to [replacement]; all other codepoints are copied without
        translation. *)

    val filter: f:(codepoint -> bool) -> t -> t
    (** [filter ~f t] creates a slice from [t]'s codepoints filtered by [f]. Only codepoints for
        which [f] returns [true] are incorporated into the result. *)

    val join: ?sep:t -> t list -> t
    (** [join ~sep slices] creates a slice comprised of the concatenation of the [slices] list, with
        [sep] interposed between the inputs. *)

    val join_rev: ?sep:t -> t list -> t
    (** [join_rev ~sep slices] creates a slice comprised of the concatenation of reversed [slices]
        list, with [sep] interposed between the inputs. *)

    val join_map: ?sep:t -> f:(codepoint -> t) -> t -> t
    (** [join_map ~sep ~f t] creates a slice which is the concatenation of applying [f] to convert
        each codepoint to a slice. This is more general (and more expensive) than {!map}, because
        each input codepoint can be mapped to any length of output. *)

    val escaped: t -> t
    (** Convert all unprintable or special ASCII characters to their backslash-escaped forms, such
        that the result represents a syntactically valid source code string. *)

    val rev: t -> t
    (** [rev t] creates a slice with the codepoint ordering reversed relative to [t]. *)

    val lfind: codepoint -> t -> Cursor.t option
    (** [lfind cp t] returns a cursor to the leftmost instance of [cp] in [t], or [None] if [cp] is
        absent. *)

    val lfind_hlt: codepoint -> t -> Cursor.t
    (** [lfind_hlt cp t] returns a cursor to the leftmost instance of [cp] in [t], or halts if [cp]
        is absent. *)

    val contains: codepoint -> t -> bool
    (** [contains cp t] returns [true] if [t] contains [cp], [false] otherwise. *)

    val rfind: codepoint -> t -> Cursor.t option
    (** [rfind cp t] returns a cursor to the rightmost instance of [cp] in [t], or [None] if [cp] is
        absent. *)

    val rfind_hlt: codepoint -> t -> Cursor.t
    (** [rfind_hlt cp t] returns a cursor to the rightmost instance of [cp] in [t], or halts if [cp]
        is absent. *)

    (** Simple but efficient pattern matching, based on the {{:
        https://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm} Knuth-Morris-Pratt algorithm}.
        Patterns are uninterpreted codepoint sequences. Searches require at most a single pass over
        the input, regardless of whether finding one or more (optionally overlapping) matches. *)
    module Pattern : sig
      type outer = t

      type t
      (** Compiled pattern. *)

      val create: outer -> t
      (** [create s] creates a compiled pattern corresponding to [s]. *)

      val find: in_:outer -> t -> Cursor.t option
      (** [find ~in_ t] returns a cursor to the leftmost match in [in_], or [None] if no match
          exists. *)

      val find_hlt: in_:outer -> t -> Cursor.t
      (** [find_hlt ~in_ t] returns a cursor to the leftmost match in [in_], or halts if no match
          exists. *)

      val find_all: may_overlap:bool -> in_:outer -> t -> Cursor.t list
      (** [find_all ~may_overlap ~in_ t] returns a list of cursors to the matches in [in_].
          Non-leftmost overlapping matches are excluded if [may_overlap] is [false]. *)

      val replace_first: in_:outer -> with_:outer -> t -> outer
      (** [replace_first ~in_ ~with_ t] returns a slice with the first match in [in_], if any,
          replaced with [with_] in the result. *)

      val replace_all: in_:outer -> with_:outer -> t -> outer
      (** [replace_all ~in_ ~with_ t] returns a slice with all matches in [in_], if any, replaced
          with [with_] in the result. *)
    end

    val is_prefix: prefix:t -> t -> bool
    (** [is_prefix ~prefix t] returns true if [prefix] is a prefix of [t]. *)

    val is_suffix: suffix:t -> t -> bool
    (** [is_suffix ~suffix t] returns true if [suffix] is a suffix of [t]. *)

    val prefix: uns -> t -> t
    (** [prefix i t] returns the prefix of [t] comprising [i] codepoints. *)

    val suffix: uns -> t -> t
    (** [suffix i t] returns the suffix of [t] comprising [i] codepoints. *)

    val chop_prefix: prefix:t -> t -> t option
    (** [chop_prefix ~prefix t] returns [t] absent [prefix], or [None] if [prefix] is not a valid
        prefix of [t]. *)

    val chop_prefix_hlt: prefix:t -> t -> t
    (** [chop_prefix_hlt ~prefix t] returns [t] absent [prefix], or halts if [prefix] is not a valid
        prefix of [t]. *)

    val chop_suffix: suffix:t -> t -> t option
    (** [chop_suffix ~suffix t] returns [t] absent [suffix], or [None] if [suffix] is not a valid
        suffix of [t]. *)

    val chop_suffix_hlt: suffix:t -> t -> t
    (** [chop_suffix_hlt ~suffix t] returns [t] absent [suffix], or halts if [suffix] is not a valid
        suffix of [t]. *)

    val lstrip: ?drop:(codepoint -> bool) -> t -> t
    (** [lstrip ~drop t] returns [t] absent the prefix codepoints for which [drop] returns [true].
        The default [drop] strips tab (['\t']), newline (['\n']), carriage return (['\r']), and
        space ([' ']). *)

    val rstrip: ?drop:(codepoint -> bool) -> t -> t
    (** [rstrip ~drop t] returns [t] absent the suffix codepoints for which [drop] returns [true].
        The default [drop] strips tab (['\t']), newline (['\n']), carriage return (['\r']), and
        space ([' ']). *)

    val strip: ?drop:(codepoint -> bool) -> t -> t
    (** [strip ~drop t] returns [t] absent the prefix and suffix codepoints for which [drop] returns
        [true]. The default [drop] strips tab (['\t']), newline (['\n']), carriage return (['\r']),
        and space ([' ']). *)

    val split_fold_until: init:'accum -> on:(codepoint -> bool) -> f:('accum -> t -> 'accum * bool)
      -> t -> 'accum
    (** [split_fold_until ~init ~on ~f t] splits [t] on [on] into slices, which [f] folds in left to
        right order based on initial value [init], until [f] returns [accum, true], or until all
        slices have been folded. *)

    val split_fold_right_until: init:'accum -> on:(codepoint -> bool)
      -> f:('accum -> t -> 'accum * bool) -> t -> 'accum
    (** [split_fold_right_until ~init ~on ~f t] splits [t] on [on] into slices, which [f] folds in
        right to left order based on initial value [init], until [f] returns [accum, true], or until
        all slices have been folded. *)

    val split_fold: init:'accum -> on:(codepoint -> bool) -> f:('accum -> t -> 'accum) -> t
      -> 'accum
    (** [split_fold ~init ~on ~f t] splits [t] on [on] into slices, which [f] folds in left to right
        order based on initial value [init]. *)

    val split_fold_right: init:'accum -> on:(codepoint -> bool) -> f:('accum -> t -> 'accum) -> t
      -> 'accum
    (** [split_fold_right ~init ~on ~f t] splits [t] on [on] into slices, which [f] folds in right
        to left order based on initial value [init]. *)

    val lines_fold: init:'accum -> f:('accum -> t -> 'accum) -> t -> 'accum
    (** [lines_fold ~init ~f t] splits [t] into lines separated by ["\r\n"] or ["\n"], which [f]
        folds in left to right order based on initial value [init]. *)

    val lines_fold_right: init:'accum -> f:('accum -> t -> 'accum) -> t -> 'accum
    (** [lines_fold_right ~init ~f t] splits [t] into lines separated by ["\r\n"] or ["\n"], which
        [f] folds in right to left order based on initial value [init]. *)

    val lsplit2: on:codepoint -> t -> (t * t) option
    (** [lsplit2 ~on t] splits [t] into two slices at the leftmost codepoint for which [on] returns
        [true], or returns [None] if [on] never returns [true]. *)

    val lsplit2_hlt: on:codepoint -> t -> t * t
    (** [lsplit2_hlt ~on t] splits [t] into two slices at the leftmost codepoint for which [on]
        returns [true], or halts if [on] never returns [true]. *)

    val rsplit2: on:codepoint -> t -> (t * t) option
    (** [rsplit2 ~on t] splits [t] into two slices at the rightmost codepoint for which [on] returns
        [true], or returns [None] if [on] never returns [true]. *)

    val rsplit2_hlt: on:codepoint -> t -> t * t
    (** [rsplit2_hlt ~on t] splits [t] into two slices at the rightmost codepoint for which [on]
        returns [true], or halts if [on] never returns [true]. *)

    (** Slice comparison operators. *)
    module O : sig
      type nonrec t = t

      include CmpableIntf.SMonoInfix with type t := t
    end
  end
end

(** Functors for converting various sequences to strings. *)
module Seq : sig
  type outer = t
  module type S = sig
    type t
    val to_string: t -> outer
  end

  (** Efficiently convert a codepoint sequence with known blength to a string. The length function
      returns blength of the remaining sequence; the next function returns the next codepoint which
      is converted to bytes in to_string. *)
  module Codepoint : sig
    module Make (T : SeqIntf.IMonoDef with type elm := codepoint) :
      S with type t := T.t
    module MakeRev (T : SeqIntf.IMonoDef with type elm := codepoint) :
      S with type t := T.t
  end

  (** Efficiently convert a string slice sequence with known blength to a string. The length
      function returns blength of the remaining sequence; the next function returns (base, past)
      cursors for the next string slice which is copied into to_string. *)
  module Slice : sig
    module Make (T : SeqIntf.IMonoDef with type elm := C.Slice.t) :
      S with type t := T.t
    module MakeRev (T : SeqIntf.IMonoDef with type elm := C.Slice.t) :
      S with type t := T.t
  end

  (** Efficiently convert a string sequence with known blength to a string. The length function
      returns blength of the remaining sequence; the next function returns the next string which is
      copied into to_string. *)
  module String : sig
    module Make (T : SeqIntf.IMonoDef with type elm := string) :
      S with type t := T.t
    module MakeRev (T : SeqIntf.IMonoDef with type elm := string) :
      S with type t := T.t
  end
end

val to_string: ?alt:bool -> ?pretty:bool -> string -> string
(** [to_string ~alt ~pretty s] returns [s] if [~pretty=false], or creates a string based on [s] that
    is a syntactically valid string token if [~pretty=true]. The output is wrapped by ["..."] and
    special characters escaped if [~alt=false], or wrapped by [``...``] if [~alt=true]. *)

val fmt: ?pad:codepoint -> ?just:Fmt.just -> ?alt:bool -> ?width:uns -> ?pretty:bool -> string
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [fmt ~pad ~just ~alt ~width s formatter] calls [formatter.fmt ~pad ~just ~width] on the result
    of [to_string ~alt ~pretty s]. *)

(* Exposed for testing purposes only. *)
val slice_pattern_pp: C.Slice.Pattern.t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

(** Formatter. *)
module Fmt : sig
  val empty: (module Fmt.Formatter)
  (** Empty formatter. *)
end

val init: ?blength:uns -> range -> f:(uns -> codepoint) -> t
(** [init ~blength crange ~f:(fun i -> ...)] creates a string of given byte length [~blength] and
    codepoint length [Range.length crange] using [~f] to map range elements to codepoints. [blength]
    must be accurate if specified. *)

val of_codepoint: codepoint -> t
(** Create a string containing a single codepoint. *)

val of_list: ?blength:uns -> codepoint list -> t
(** [of_list ~blength codepoints] creates a string of given byte length containing the ordered
    [codepoints]. [blength] must be accurate if specified. *)

val of_list_rev: ?blength:uns -> codepoint list -> t
(** [of_list_rev ~blength codepoints] creates a string of given byte length containing the
    reverse-ordered [codepoints]. [blength] must be accurate if specified. *)

val of_array: ?blength:uns -> codepoint array -> t
(** [of_array ~blength codepoints] creates a string of given byte length containing the ordered
    [codepoints]. [blength] must be accurate if specified. *)

include ContainerIntf.SMonoIndex with type t := t with type elm := codepoint
include ContainerIntf.SMonoMem with type t := t with type elm := codepoint

val length: t -> uns [@@ocaml.deprecated "Use [bc]length instead"]
(** Use {!blength} instead of [length], to keep the difference between byte length and codepoint
    length explicit. *)

val map: f:(codepoint -> codepoint) -> t -> t
(** [map ~f t] creates a string with codepoints mapped from [t]'s codepoints. *)

val mapi: f:(uns -> codepoint -> codepoint) -> t -> t
(** [mapi ~f t] creates a string with codepoints mapped from [t]'s codepoints. Codepoint index
    within [t] is provided to [f]. *)

val tr: target:codepoint -> replacement:codepoint -> t -> t
(** [tr ~target ~replacement t] creates a string with select codepoints translated from [t]'s
    codepoints. [target] is translated to [replacement]; all other codepoints are copied without
    translation. *)

val filter: f:(codepoint -> bool) -> t -> t
(** [filter ~f t] creates a string from [t]'s codepoints filtered by [f]. Only codepoints for which
    [f] returns [true] are incorporated into the result. *)

val join: ?sep:t -> t list -> t
(** [join ~sep strings] creates a string comprised of the concatenation of the [strings] list, with
    [sep] interposed between the inputs. *)

val join_rev: ?sep:t -> t list -> t
(** [join_rev ~sep strings] creates a string comprised of the concatenation of reversed [strings]
    list, with [sep] interposed between the inputs. *)

val join_map: ?sep:t -> f:(codepoint -> t) -> t -> t
(** [join_map ~sep ~f t] creates a string which is the concatenation of applying [f] to convert each
    codepoint to a string. This is more general (and more expensive) than {!map}, because each input
    codepoint can be mapped to any length of output. *)

val escaped: t -> t
(** Convert all unprintable or special ASCII characters to their backslash-escaped forms, such that
    the result represents a syntactically valid source code string. *)

val rev: t -> t
(** [rev t] creates a string with the codepoint ordering reversed relative to [t]. *)

val ( ^ ): t -> t -> t
(** [s0 ^ s1] is equivalent to [join [a; b]]. *)

val lfind: ?base:C.Cursor.t -> ?past:C.Cursor.t -> codepoint -> t -> C.Cursor.t option
(** [lfind cp t] returns a cursor to the leftmost instance of [cp] in [t], or [None] if [cp] is
    absent. *)

val lfind_hlt: ?base:C.Cursor.t -> ?past:C.Cursor.t -> codepoint -> t -> C.Cursor.t
(** [lfind_hlt cp t] returns a cursor to the leftmost instance of [cp] in [t], or halts if [cp] is
    absent. *)

val contains: ?base:C.Cursor.t -> ?past:C.Cursor.t -> codepoint -> t -> bool
(** [contains cp t] returns [true] if [t] contains [cp], [false] otherwise. *)

val rfind: ?base:C.Cursor.t -> ?past:C.Cursor.t -> codepoint -> t -> C.Cursor.t option
(** [rfind cp t] returns a cursor to the rightmost instance of [cp] in [t], or [None] if [cp] is
    absent. *)

val rfind_hlt: ?base:C.Cursor.t -> ?past:C.Cursor.t -> codepoint -> t -> C.Cursor.t
(** [rfind_hlt cp t] returns a cursor to the rightmost instance of [cp] in [t], or halts if [cp] is
    absent. *)

val substr_find: ?base:C.Cursor.t -> pattern:t -> t -> C.Cursor.t option
(** [substr_find ~base ~pattern t] returns a cursor to the leftmost [pattern] match past [base] in
    [t], or [None] if no match exists. [base] defaults to the beginning of [t]. *)

val substr_find_hlt: ?base:C.Cursor.t -> pattern:t -> t -> C.Cursor.t
(** [substr_find_hlt ~base ~pattern t] returns a cursor to the leftmost [pattern] match past [base]
    in [t], or halts if no match exists. [base] defaults to the beginning of [t]. *)

val substr_find_all: may_overlap:bool -> pattern:t -> t -> C.Cursor.t list
(** [substr_find_all ~may_overlap ~pattern t] returns a list of cursors to the [pattern] matches in
    [t]. Non-leftmost overlapping matches are excluded if [may_overlap] is [false]. *)

val substr_replace_first: ?base:C.Cursor.t -> pattern:t -> with_:t -> t -> t
(** [subst_replace_first ~base ~pattern ~with_ t] returns a string with the first [pattern] match
    past [base] in [t], if any, replaced with [with_] in the result. [base] defaults to the
    beginning of [t]. *)

val substr_replace_all: pattern:t -> with_:t -> t -> t
(** [substr_replace_all ~pattern ~with_ t] returns a string with all [pattern] matches in [t], if
    any, replaced with [with_] in the result. *)

val is_prefix: prefix:t -> t -> bool
(** [is_prefix ~prefix t] returns true if [prefix] is a prefix of [t]. *)

val is_suffix: suffix:t -> t -> bool
(** [is_suffix ~suffix t] returns true if [suffix] is a suffix of [t]. *)

val pare: base:C.Cursor.t -> past:C.Cursor.t -> t -> t
(** [pare ~base ~past t] returns a string comprised of the codepoint sequence in [\[base .. past)]
    of [t]. *)

val prefix: uns -> t -> t
(** [prefix i t] returns the prefix of [t] comprising [i] codepoints. *)

val suffix: uns -> t -> t
(** [suffix i t] returns the suffix of [t] comprising [i] codepoints. *)

val chop_prefix: prefix:t -> t -> t option
(** [chop_prefix ~prefix t] returns [t] absent [prefix], or [None] if [prefix] is not a valid prefix
    of [t]. *)

val chop_prefix_hlt: prefix:t -> t -> t
(** [chop_prefix_hlt ~prefix t] returns [t] absent [prefix], or halts if [prefix] is not a valid
    prefix of [t]. *)

val chop_suffix: suffix:t -> t -> t option
(** [chop_suffix ~suffix t] returns [t] absent [suffix], or [None] if [suffix] is not a valid suffix
    of [t]. *)

val chop_suffix_hlt: suffix:t -> t -> t
(** [chop_suffix_hlt ~suffix t] returns [t] absent [suffix], or halts if [suffix] is not a valid
    suffix of [t]. *)

val lstrip: ?drop:(codepoint -> bool) -> t -> t
(** [lstrip ~drop t] returns [t] absent the prefix codepoints for which [drop] returns [true]. The
    default [drop] strips tab (['\t']), newline (['\n']), carriage return (['\r']), and space
    ([' ']). *)

val rstrip: ?drop:(codepoint -> bool) -> t -> t
(** [rstrip ~drop t] returns [t] absent the suffix codepoints for which [drop] returns [true]. The
    default [drop] strips tab (['\t']), newline (['\n']), carriage return (['\r']), and space
    ([' ']). *)

val strip: ?drop:(codepoint -> bool) -> t -> t
(** [strip ~drop t] returns [t] absent the prefix and suffix codepoints for which [drop] returns
    [true]. The default [drop] strips tab (['\t']), newline (['\n']), carriage return (['\r']), and
    space ([' ']). *)

val split: f:(codepoint -> bool) -> t -> t list
(** [split ~f t] splits [t] on codepoints for which [f] returns true, into a list of strings. *)

val split_rev: f:(codepoint -> bool) -> t -> t list
(** [split_rev ~f t] splits [t] on codepoints for which [f] returns true, into a reversed list of
    strings. *)

val split_lines: t -> t list
(** [split_lines t] splits [t] by ["\r\n"] or ["\n"] into a list of lines. *)

val split_lines_rev: t -> t list
(** [split_lines_rev t] splits [t] by ["\r\n"] or ["\n"] into a reversed list of lines. *)

val lsplit2: on:codepoint -> t -> (t * t) option
(** [lsplit2 ~on t] splits [t] into two strings at the leftmost codepoint for which [on] returns
    [true], or returns [None] if [on] never returns [true]. *)

val lsplit2_hlt: on:codepoint -> t -> t * t
(** [lsplit2_hlt ~on t] splits [t] into two strings at the leftmost codepoint for which [on] returns
    [true], or halts if [on] never returns [true]. *)

val rsplit2: on:codepoint -> t -> (t * t) option
(** [rsplit2 ~on t] splits [t] into two strings at the rightmost codepoint for which [on] returns
    [true], or returns [None] if [on] never returns [true]. *)

val rsplit2_hlt: on:codepoint -> t -> t * t
(** [rsplit2_hlt ~on t] splits [t] into two strings at the rightmost codepoint for which [on]
    returns [true], or halts if [on] never returns [true]. *)

(** String comparison operators. *)
module O : sig
  type nonrec t = t

  include CmpableIntf.SMonoInfix with type t := t
end
