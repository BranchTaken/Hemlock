(** Stringable type functor signature. *)

module type S = sig
  type t

  val of_string: string -> t
  (** Convert {!type:string} to [t]. *)

  val to_string: t -> string
  (** Convert [t] to {!type:string}. *)
end
