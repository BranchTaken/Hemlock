(** UTF-8-encoded code point. This module provides mechanisms for validated
    encoding conversion between code points and bytes, but is intended for
    ephemeral use. Most operations on code points are performed via the
    {!type:codepoint} and {!type:string} types. *)

open Rudiments0

type t

include Cmpable_intf.S_mono with type t := t
include Formattable_intf.S_mono with type t := t

val of_codepoint: codepoint -> t
(** Initialize from {!type:codepoint}. *)

val to_codepoint: t -> codepoint
(** Convert to {!type:codepoint}. *)

val to_bytes: t -> byte list
(** Convert to a byte list. *)

val length: t -> uns
(** Return the length of the UTF-8 code point in bytes. *)

val to_string: t -> string
(** [to_string t] returns a UTF-8-encoded string representation of [t]. *)

val escape: t -> string
(** [escape t] returns a syntactically valid UTF-8-encoded string representation
    of [t]. *)

(** Functors for converting UTF-8-encoded byte sequences to {!type:codepoint}
    values. *)
module Seq : sig
  type outer = t
  module type S = sig
    type t

    val to_codepoint: t -> (codepoint option * t) option
    (** Convert beginning of sequence to a {!type:codepoint} ([None] upon
        encountering a UTF-8 encoding error) and return it along with the
        sequence remainder, or return [None] if the sequence is empty. *)

    val to_codepoint_replace: t -> (codepoint * t) option
    (** Convert beginning of sequence to a {!type:codepoint} (replacing UTF-8
        encoding errors with '�') and return it along with the sequence
        remainder, or return [None] if the sequence is empty. *)

    val to_codepoint_hlt: t -> (codepoint * t) option
    (** Convert beginning of sequence to a {!type:codepoint} (halt on UTF-8
        encoding errors) and return it along with the sequence remainder, or
        return [None] if the sequence is empty. *)
  end

  (** Iteratively convert a UTF-8-encoded byte sequence to {!type:codepoint}
      values. *)
  module Make (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t

  (** Iteratively convert a reversed UTF-8-encoded byte sequence to
      {!type:codepoint} values. *)
  module Make_rev (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t
end
