(* Module aliases. *)

module Isize = I63
module Usize = U63
module Codepoint = U21
module Byte = U8

(* Type aliases. *)

(* Redundant; ('a array) is intrinsic and equivalent to ('a Array.t). *)
(* type 'a array = 'a Array.t *)

type u128 = Rudiments_int0.u128

type i64 = Rudiments_int0.i64

type u64 = Rudiments_int0.u64

type isize = Rudiments_int0.isize

type usize = Rudiments_int0.usize
(** {!type:usize} is the default numerical type. *)

type i63 = I63.t

type u63 = U63.t

type u21 = U21.t
type codepoint = Codepoint.t

type u8 = U8.t
type byte = Byte.t

type i2 = I2.t

(* Unnecessary, due to aliasing the built-in option type. *)
(* type 'a option = 'a Option.t *)

(* Functions. *)

val not_reached: unit -> 'a
(** Hypothetically unreachable code.  [not_reached] halts if called. *)

val not_implemented: string -> 'a
(** Placeholder for unimplemented code.  [not_implemented] halts if called. *)

val halt: string -> 'a
(** [halt s] prints [s] to [stderr] and halts the actor. *)

val demand: bool -> unit
(** Like [assert], but cannot be disabled. *)

val usize_of_isize: isize -> usize
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val isize_of_usize: usize -> isize
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val int_of_isize: isize -> int
(** Convert a signed integer to a bitwise identical OCaml integer. *)

val isize_of_int: int -> isize
(** Convert an OCaml integer to a bitwise identical signed integer. *)

include Intnb_intf.S_u with type t := usize

val not: bool -> bool
(** [not t] returns the logical negation of [t]. *)
