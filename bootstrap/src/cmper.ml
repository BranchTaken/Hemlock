(* The redundancy between the .mli and .ml files cannot be extracted into an
   interface file, apparently because the phantom witness type somehow causes
   two incompatible-but-identical comparator types to exist. *)

type ('a, 'witness) t = {
  cmp: 'a -> 'a -> Cmp.t;
  pp: Format.formatter -> 'a -> unit
}

module type I_mono = sig
  type t
  include Cmpable_intf.I_mono with type t := t
  include Formattable_intf.S_mono with type t := t
end

module type S_mono = sig
  type ('a, 'witness) cmper = ('a, 'witness) t
  type t
  type cmper_witness
  val cmper: (t, cmper_witness) cmper
end

module Make_mono (T : I_mono) : S_mono with type t := T.t = struct
  type ('a, 'witness) cmper = ('a, 'witness) t
  type cmper_witness
  let cmper = T.{cmp; pp}
end

module type I_poly = sig
  type 'a t
  include Cmpable_intf.I_poly with type 'a t := 'a t
  include Formattable_intf.S_poly with type 'a t := 'a t
  val pp_a: Format.formatter -> 'a -> unit
end

module type S_poly = sig
  type ('a, 'witness) cmper = ('a, 'witness) t
  type 'a t
  type cmper_witness
  val cmper: ('a t, cmper_witness) cmper
end

module Make_poly (T : I_poly) : S_poly with type 'a t := 'a T.t = struct
  type ('a, 'witness) cmper = ('a, 'witness) t
  type cmper_witness

  let pp ppf t =
    T.pp T.pp_a ppf t

  let cmper = {cmp=T.cmp; pp}
end
