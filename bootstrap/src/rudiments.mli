(* Aliases. *)

module Int = I63
module Uint = U63
module Codepoint = U21
module Byte = U8

(* Types. *)

type uint = Rudiments_uint.uint

(* Redundant; ('a array) is intrinsic and equivalent to ('a Array.t). *)
(* type 'a array = 'a Array.t *)

type i63 = I63.t
(* Redundant; int is intrinsic and equivalent to Int.t . *)
(* type int = Int.t *)

type u63 = U63.t

type u21 = U21.t
type codepoint = Codepoint.t

type u8 = U8.t
type byte = Byte.t

(* Functions. *)

val not_reached: unit -> 'a
val not_implemented: string -> 'a
val halt: string -> 'a

(* (int) is the default numerical type. *)
include Intnb_intf.S_i with type t := int

(* (uint) is abstract, and therefore incompatible with (int). *)
val uint_of_int: int -> uint
val int_of_uint: uint -> int
