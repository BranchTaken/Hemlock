(* Types. *)

type 'a array = 'a Array.t

type i63 = I63.t
module Int = I63
type int = Int.t

type u63 = U63.t
module Uint = U63
type uint = Uint.t

type u8 = U8.t
module Byte = U8
type byte = Byte.t

(* Functions. *)

val not_reached: unit -> 'a
val not_implemented: string -> 'a
val halt: string -> 'a
