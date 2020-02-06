include Hashable_intf

type 'a t = {
  hash_fold: 'a -> Hash.State.t -> Hash.State.t;
  cmp: 'a -> 'a -> Cmp.t;
}

let of_key (type a) (module Key: Key with type t = a) =
  {
    hash_fold = Key.hash_fold;
    cmp = Key.cmp;
  }

let to_key (type a) {cmp; hash_fold} =
  (
    module struct
      type t = a
      let hash_fold = hash_fold
      let cmp = cmp
    end
    : Key with type t = a
  )
