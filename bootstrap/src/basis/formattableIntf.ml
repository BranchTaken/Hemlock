(** Formattable type functor signature. *)

(** Monomorphic Fmt pretty printing function. *)
module type SMono = sig
  type t

  val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
  (** [pp t formatter] applies a formatted representation of [t] to the [formatter]. *)
end

(** Polymorphic Fmt pretty printing function. *)
module type SPoly = sig
  type 'a t

  val pp: ('a -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> 'a t
    -> (module Fmt.Formatter) -> (module Fmt.Formatter)
    (** [pp pp_a t formatter] applies a formatted representation of [t] to the [formatter] using
        [pp_a] for the parametric type value [a]. *)
end

(** Polymorphic Fmt pretty printing function. *)
module type SPoly2 = sig
  type ('a, 'b) t

  val pp: ('a -> (module Fmt.Formatter) -> (module Fmt.Formatter))
    -> ('b -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> ('a, 'b) t ->
    (module Fmt.Formatter) -> (module Fmt.Formatter)
    (** [pp pp_a pp_b t formatter] applies a formatted representation of [t] to the [formatter]
        using [pp_a] for the parametric type value [a], and [pp_b] for the parametric type value
        [b]. *)
end
