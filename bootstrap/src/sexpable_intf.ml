(** Sexp conversion functions. *)
module type S = sig
  type t

  val t_of_sexp: Sexplib0.Sexp.t -> t
  (** Convert {!type:Sexp.t} to [t]. *)

  val sexp_of_t: t -> Sexplib0.Sexp.t
  (** Convert [t] to {!type:Sexp.t}. *)
end
