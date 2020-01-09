(** Formattable type functor signature. *)

(** Monomorphic Format pretty printing conversion functions. *)
module type S_mono = sig
  type t

  val pp: Format.formatter -> t -> unit
  (** [pp ppf t] prints a representation of [t] to the pretty printing
      formatter, [ppf].  This function is intended for use with the [%a] format
      specifier to {!Format.printf}.*)
end

(** Polymorphic Format pretty printing conversion functions. *)
module type S_poly = sig
  type 'a t

  val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** [pp pp_a ppf t] prints a syntactic representation of [t] to the pretty
      printing formatter, [ppf], using the [pp_a] printer for the parametric
      type value [a].  This function is intended for use with the [%a] format
      specifier to {!Format.printf}. *)
end

(** Polymorphic Format pretty printing conversion functions. *)
module type S_poly2 = sig
  type ('a, 'b) t

  val pp: (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit)
    -> Format.formatter -> ('a, 'b) t -> unit
  (** [pp pp_a bb_b ppf t] prints a syntactic representation of [t] to the
      pretty printing formatter, [ppf], using the [pp_a] printer for the
      parametric type value [a], and [pp_b] for the parametric type value [b].
      This function is intended for use with the [%a] format specifier to
      {!Format.printf}. *)
end
