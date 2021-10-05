(* Type aliases. *)

(* Redundant; ('a array) is intrinsic and equivalent to ('a Array.t). *)
(* type 'a array = 'a Array.t *)

type u128 = RudimentsInt0.u128

type i64 = RudimentsInt0.i64

type u64 = RudimentsInt0.u64

type sint = RudimentsInt0.sint
(** {!type:sint} would ideally be named {!type:int}, but it is important that {!type:sint} be
    incompatible with {!type:uns}, and it would be difficult to maintain this incompatibility with
    {!type:int} because {!type:uns} must be visibly equivalent to OCaml's built-in {!type:int}. *)

type uns = RudimentsInt0.uns
(** {!type:uns} is the default numerical type. *)

type i32 = I32.t

type u32 = U32.t

type i16 = I16.t

type u16 = U16.t

type i8 = I8.t

type u8 = U8.t
type byte = Byte.t

type codepoint = Codepoint.t

type real = float

(* Unnecessary, due to aliasing the built-in option type. *)
(* type 'a option = 'a Option.t *)

(* Functions. *)

val not_reached: unit -> 'a
(** Hypothetically unreachable code. [not_reached] halts if called. *)

val not_implemented: string -> 'a
(** Placeholder for unimplemented code. [not_implemented] halts if called. *)

val halt: string -> 'a
(** [halt s] prints [s] to [stderr] and halts the actor. *)

val uns_of_sint: sint -> uns
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val sint_of_uns: uns -> sint
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val int_of_sint: sint -> int
(** Convert a signed integer to a narrowed OCaml integer. *)

val sint_of_int: int -> sint
(** Convert an OCaml integer to a widened signed integer. *)

include (module type of Uns)

val not: bool -> bool
(** [not t] returns the logical negation of [t]. *)
