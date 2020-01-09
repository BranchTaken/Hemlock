(* Module aliases. *)

module Int = I63
module Uint = U63
module Codepoint = U21
module Byte = U8

(* Type aliases. *)

(* Redundant; ('a array) is intrinsic and equivalent to ('a Array.t). *)
(* type 'a array = 'a Array.t *)

type uint = Rudiments_uint0.uint
(** {!type:uint} is the default numerical type. *)

type i63 = I63.t
(* Redundant; int is intrinsic and equivalent to Int.t . *)
(* type int = Int.t *)

type u63 = U63.t

type u21 = U21.t
type codepoint = Codepoint.t

type u8 = U8.t
type byte = Byte.t

(* Unnecessary, due to aliasing the built-in option type. *)
(* type 'a option = 'a Option.t *)

(* Functions. *)

val not_reached: unit -> 'a
(** Hypothetically unreachable code.  [not_reached] halts if called. *)

val not_implemented: string -> 'a
(** Placeholder for unimplemented code.  [not_implemented] halts if called. *)

val halt: string -> 'a
(** [halt s] prints [s] to [stderr] and halts the actor. *)

val uint_of_int: int -> uint
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val int_of_uint: uint -> int
(** Convert an unsigned integer to a bitwise identical unsigned integer. *)

val kv: int -> uint
(** Create a constant value. *)

include Intnb_intf.S_u with type t := uint
