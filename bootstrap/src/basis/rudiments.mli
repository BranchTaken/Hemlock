(* Module aliases. *)

module Sint = I63
module Uns = U63
module Codepoint = U21
module Byte = U8

(* Type aliases. *)

(* Redundant; ('a array) is intrinsic and equivalent to ('a Array.t). *)
(* type 'a array = 'a Array.t *)

type u128 = Rudiments_int0.u128

type i64 = Rudiments_int0.i64

type u64 = Rudiments_int0.u64

type sint = Rudiments_int0.sint
(** {!type:sint} would ideally be named {!type:int}, but it is important that
    {!type:sint} be incompatible with {!type:uns}, and it would be difficult to
    maintain this incompatibility with {!type:int} because {!type:uns} must be
    visibly equivalent to OCaml's built-in {!type:int}.  *)

type uns = Rudiments_int0.uns
(** {!type:uns} is the default numerical type. *)

type i63 = I63.t

type u63 = U63.t

type u32 = U32.t

type u21 = U21.t
type codepoint = Codepoint.t

type u16 = U16.t

type u8 = U8.t
type byte = Byte.t

type i2 = I2.t

(* Unnecessary, due to aliasing the built-in option type. *)
(* type 'a option = 'a Option.t *)

(* Functions. *)

val not_reached: unit -> 'a
(** Hypothetically unreachable code. [not_reached] halts if called. *)

val not_implemented: string -> 'a
(** Placeholder for unimplemented code. [not_implemented] halts if called. *)

val halt: string -> 'a
(** [halt s] prints [s] to [stderr] and halts the actor. *)

val demand: bool -> unit
(** Like [assert], but cannot be disabled. *)

val uns_of_sint: sint -> uns
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val sint_of_uns: uns -> sint
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val int_of_sint: sint -> int
(** Convert a signed integer to a bitwise identical OCaml integer. *)

val sint_of_int: int -> sint
(** Convert an OCaml integer to a bitwise identical signed integer. *)

include Intnb_intf.S_u with type t := uns

val not: bool -> bool
(** [not t] returns the logical negation of [t]. *)
