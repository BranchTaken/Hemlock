open Hashable_intf

type 'a t = {
  hash: 'a -> Hash.t;
  cmp: 'a -> 'a -> Cmp.t;
  sexp_of_t: 'a -> Sexplib0.Sexp.t;
}

val of_key: (module Key with type t = 'a) -> 'a t
val to_key: 'a t -> (module Key with type t = 'a)
