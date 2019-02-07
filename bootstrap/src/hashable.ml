module type Key = sig
  type t
  val hash: t -> Hash.t
  val cmp: t -> t -> Cmp.t
  val sexp_of_t: t -> Sexplib0.Sexp.t
end

type 'a t = {
  hash: 'a -> Hash.t;
  cmp: 'a -> 'a -> Cmp.t;
  sexp_of_t: 'a -> Sexplib0.Sexp.t;
}

let of_key (type a) (module Key: Key with type t = a) =
  {
    hash = Key.hash;
    cmp = Key.cmp;
    sexp_of_t = Key.sexp_of_t;
  }

let to_key (type a) {hash; cmp; sexp_of_t} =
  (
    module struct
      type t = a
      let hash = hash
      let cmp = cmp
      let sexp_of_t = sexp_of_t
    end
    : Key with type t = a
  )
