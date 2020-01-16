(** Hash type. *)

open Rudiments_uint0

type t
(** Hash type. *)

type state
(** Hash state type. *)

val state_of_uint: uint -> state
(** Initialize hash state. *)

val t_of_state: state -> t
(** Return hash value associated with state. *)

val s_fmt: unit -> t -> string
(** [s_fmt () t] returns a string decimal representation of [t], for use with
    the [%a] format specifier to {!Printf.sprintf}. *)

val s_fmt_hex: unit -> t -> string
(** [s_fmt_hex () t] returns a string hexadecimal representation of [t], for
    use with the [%a] format specifier to {!Printf.sprintf}. *)

val fmt: out_channel -> t -> unit
(** [fmt oc t] outputs a string decimal representation of [t] to [oc], for use
    with the [%a] format specifier to {!Printf.printf}. *)

val fmt_hex: out_channel -> t -> unit
(** [fmt oc t] outputs a string hexadecimal representation of [t] to [oc], for
    use with the [%a] format specifier to {!Printf.printf}. *)

val hash_fold: state -> 'a -> state
(** [hash_fold state a] incorporates the hash of [a] into [state] and returns
    the resulting state. *)

val hash: 'a -> t
(** [hash a] returns the hash of [a]. *)
