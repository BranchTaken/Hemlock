(** 21-bit unsigned integer. *)

(* Partial Rudiments. *)
module Int = I63
module Uint = U63
type uint = Uint.t

type t
include Intnb_intf.S_u with type t := t

val to_int: t -> int
(** Convert to full-width signed integer. *)

val of_int: int -> t
(** Initialize from full-width signed integer, with possible loss. *)

val of_int_hlt: int -> t
(** Initialize from full-width signed integer, or halt if conversion would be
    lossy. *)

val kv: int -> t
(** Create constant value.  This is a stopgap solution for the lack of
    bitwidth-specific literals. *)

val to_uint: t -> uint
(** Convert to full-width unsigned integer. *)

val of_uint: uint -> t
(** Initialize from full-width unsigned integer, with possible loss. *)

val of_uint_hlt: uint -> t
(** Initialize from full-width unsigned integer, or halt if conversion would be
    lossy. *)

val of_char: char -> t
(** Initialize from character literal.  This is a stopgap for the lack of
    codepoint literals. *)

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
(** Constant ['\b']. *)

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
