module type Key = sig
  type t
  val hash_fold: Hash.state -> t -> Hash.state
  val cmp: t -> t -> Cmp.t
end
