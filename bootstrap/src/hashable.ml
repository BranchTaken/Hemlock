module type Key = sig
  type t
  val hash_fold: Hash.state -> t -> Hash.state
  val cmp: t -> t -> Cmp.t
end

type 'a t = {
  hash_fold: Hash.state -> 'a -> Hash.state;
  cmp: 'a -> 'a -> Cmp.t;
}

let of_key (type a) (module Key: Key with type t = a) =
  {
    hash_fold = Key.hash_fold;
    cmp = Key.cmp;
  }

let to_key (type a) {hash_fold; cmp} =
  (
    module struct
      type t = a
      let hash_fold = hash_fold
      let cmp = cmp
    end
    : Key with type t = a
  )
