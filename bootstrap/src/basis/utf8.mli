(** UTF-8-encoded code point.  This module provides mechanisms for validated
    encoding conversion between code points and bytes, but is intended for
    ephemeral use.  Most operations on code points are performed via the
    {!type:codepoint} and {!type:string} types. *)

open Rudiments

type t

include Cmpable_intf.S_mono with type t := t
include Formattable_intf.S_mono with type t := t

val of_codepoint: codepoint -> t
(** Initialize from [codepoint]. *)

val to_codepoint: t -> codepoint
(** Convert to [codepoint]. *)

(** Functors for converting bytes to UTF-8 code points. *)
module Seq : sig
  type outer = t
  module type S = sig
    type t

    val to_utf8: t -> ((outer, byte list) result * t) option
    (** Convert beginning of sequence to a validated UTF-8 code point and return
        it along with the sequence remainder, or return [Error] along with
        sequence remainder if the sequence is malformed, or return [None] if the
        sequence is empty. *)

    val to_utf8_hlt: t -> (outer * t) option
    (** Convert beginning of sequence to a validated UTF-8 code point and return
        it along with the sequence remainder, or halt if the sequence is
        malformed, or return [None] if the sequence is empty. *)
  end

  (** Iteratively convert a byte sequence to UTF-8 code points. *)
  module Make (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t

  (** Iteratively convert a reversed byte sequence to UTF-8 code points. *)
  module Make_rev (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t
end

val to_bytes: t -> byte list
(** Convert to a byte list. *)

val length: t -> usize
(** Return the length of the UTF-8 code point in bytes. *)

val to_string: t -> string
(** [to_string t] returns a UTF-8-encoded string representation of [t]. *)

val escape: t -> string
(** [escape t] returns a syntactically valid UTF-8-encoded string representation
    of [t]. *)
