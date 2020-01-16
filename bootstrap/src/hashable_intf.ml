(** Generic Key module capable of hashing and comparing keys. *)
module type Key = sig
  type t
  (** Type being hashed/compared. *)

  val hash_fold: Hash.state -> t -> Hash.state
  (** [hash_fold state a] incorporates the hash of [a] into [state] and returns
      the resulting state. *)

  val cmp: t -> t -> Cmp.t
  (** [cmp a b] returns [Cmp.lt] if [a < b], [Cmp.eq] if [a = b], or [Cmp.gt] if
      [a > b]. *)
end
