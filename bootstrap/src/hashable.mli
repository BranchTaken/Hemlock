open Hashable_intf

type 'a t = {
  hash_fold: Hash.state -> 'a -> Hash.state;
  cmp: 'a -> 'a -> Cmp.t;
}

val of_key: (module Key with type t = 'a) -> 'a t
val to_key: 'a t -> (module Key with type t = 'a)
