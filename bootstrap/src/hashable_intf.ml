module type Key = sig
  type t
  val hash: t -> Hash.t
  val cmp: t -> t -> Cmp.t
  val sexp_of_t: t -> Sexplib0.Sexp.t
end
