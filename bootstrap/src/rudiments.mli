(* Aliases. *)

module Int = I63
module Uint = U63
module Codepoint = U21
module Byte = U8

(* Types. *)

type 'a array = 'a Array.t

type i63 = I63.t
type int = Int.t

type u63 = U63.t
type uint = Uint.t

type u21 = U21.t
type codepoint = Codepoint.t

type u8 = U8.t
type byte = Byte.t

(* Functions. *)

val not_reached: unit -> 'a
val not_implemented: string -> 'a
val halt: string -> 'a
