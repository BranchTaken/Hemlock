(** Unicode code point. *)

open Rudiments_int
type byte = U8.t

type t

include Formattable_intf.S_mono with type t := t
include Cmpable_intf.S_mono with type t := t

val kv: int -> t
(** Create constant value. This is a stopgap solution for the lack of
    codepoint literals. *)

val to_uns: t -> uns
(** Convert to full-width unsigned integer. *)

val of_uns: uns -> t
(** Initialize from full-width unsigned integer, with possible loss. *)

val of_uns_hlt: uns -> t
(** Initialize from full-width unsigned integer, or halt if conversion would be
    lossy. *)

val to_bytes: t -> byte list
(** Convert to a byte list. *)

val of_char: char -> t
(** Initialize from character literal. This is a stopgap for the lack of
    codepoint literals. *)

val to_string: t -> string
(** [to_string t] returns a UTF-8-encoded string representation of [t]. *)

val escape: t -> string
(** [escape t] returns a syntactically valid UTF-8-encoded codepoint literal
    representation of [t]. *)

val replacement: t
(** Replacement character '�', [0xfffd]. *)

val nul: t
(** Constant [0x00]. *)

val soh: t
(** Constant [0x01]. *)

val stx: t
(** Constant [0x02]. *)

val etx: t
(** Constant [0x03]. *)

val eot: t
(** Constant [0x04]. *)

val enq: t
(** Constant [0x05]. *)

val ack: t
(** Constant [0x06]. *)

val bel: t
(** Constant [0x07]. *)

val bs: t
(** Constant [0x08]. *)

val ht: t
(** Constant ['\t']. *)

val lf: t
(** Constant ['\n']. *)

val nl: t
(** Constant ['\n']. *)

val vt: t
(** Constant [0x0b]. *)

val ff: t
(** Constant [0x0c]. *)

val cr: t
(** Constant ['\r']. *)

val so: t
(** Constant [0x0e]. *)

val si: t
(** Constant [0x0f]. *)

val dle: t
(** Constant [0x10]. *)

val dc1: t
(** Constant [0x11]. *)

val dc2: t
(** Constant [0x12]. *)

val dc3: t
(** Constant [0x13]. *)

val dc4: t
(** Constant [0x14]. *)

val nak: t
(** Constant [0x15]. *)

val syn: t
(** Constant [0x16]. *)

val etb: t
(** Constant [0x17]. *)

val can: t
(** Constant [0x18]. *)

val em: t
(** Constant [0x19]. *)

val sub: t
(** Constant [0x1a]. *)

val esc: t
(** Constant [0x1b]. *)

val fs: t
(** Constant [0x1c]. *)

val gs: t
(** Constant [0x1d]. *)

val rs: t
(** Constant [0x1e]. *)

val us: t
(** Constant [0x1f]. *)

val del: t
(** Constant [0x7f]. *)

(** UTF-8-encoded code point. This module provides mechanisms for validated
    encoding conversion between code points and bytes, but is intended for
    ephemeral use. Most operations on code points are performed via the
    {!type:codepoint} and {!type:string} types. *)
module Utf8 : sig
  type outer = t
  type t

  val of_codepoint: outer -> t
  (** Initialize from {!type:codepoint}. *)

  val to_codepoint: t -> outer
  (** Convert to {!type:codepoint}. *)

  val to_bytes: t -> byte list
  (** Convert to a byte list. *)

  val length: t -> uns
  (** Return the length of the UTF-8 code point in bytes. *)

  val to_string: t -> string
  (** [to_string t] returns a UTF-8-encoded string representation of [t]. *)

  val escape: t -> string
  (** [escape t] returns a syntactically valid UTF-8-encoded string
      representation of [t]. *)
end

(** Functors for converting UTF-8-encoded byte sequences to {!type:codepoint}
    values. *)
module Seq : sig
  type outer = t
  module type S = sig
    type t

    val to_codepoint: t -> (outer option * t) option
    (** Convert beginning of sequence to a {!type:codepoint} ([None] upon
        encountering a UTF-8 encoding error) and return it along with the
        sequence remainder, or return [None] if the sequence is empty. *)

    val to_codepoint_replace: t -> (outer * t) option
    (** Convert beginning of sequence to a {!type:codepoint} (replacing UTF-8
        encoding errors with '�') and return it along with the sequence
        remainder, or return [None] if the sequence is empty. *)

    val to_codepoint_hlt: t -> (outer * t) option
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
