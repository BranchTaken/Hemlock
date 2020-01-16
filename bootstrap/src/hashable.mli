(** Hashable key utilities. *)

open Hashable_intf

type 'a t = {
  hash_fold: Hash.state -> 'a -> Hash.state;
  (** [hash_fold state a] incorporates the hash of [a] into [state] and returns
      the resulting state. *)

  cmp: 'a -> 'a -> Cmp.t;
  (** [cmp a b] returns [Cmp.lt] if [a < b], [Cmp.eq] if [a = b], or [Cmp.gt] if
      [a > b]. *)
}

val of_key: (module Key with type t = 'a) -> 'a t
(** [of_key k] returns a {!type:t} corresponding to [k]. *)

val to_key: 'a t -> (module Key with type t = 'a)
(** [to_key t] returns a {!module:Key} corresponding to [t]. *)
