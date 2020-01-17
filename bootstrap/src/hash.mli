(** Hash type. *)

open Rudiments_uint0

type t
(** Hash type. *)

include Formattable_intf.S_mono with type t := t

type state
(** Hash state type. *)

val state_of_uint: uint -> state
(** Initialize hash state. *)

val t_of_state: state -> t
(** Return hash value associated with state. *)

val hash_fold: state -> 'a -> state
(** [hash_fold state a] incorporates the hash of [a] into [state] and returns
    the resulting state. *)

val hash: 'a -> t
(** [hash a] returns the hash of [a]. *)
