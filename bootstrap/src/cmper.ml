(* The redundancy between the .mli and .ml files cannot be extracted into an
   interface file, apparently because the phantom witness type somehow causes
   two incompatible-but-identical comparator types to exist. *)

type ('a, 'witness) t = {
  hash_fold: 'a -> Hash.State.t -> Hash.State.t;
  cmp: 'a -> 'a -> Cmp.t;
  pp: Format.formatter -> 'a -> unit
}

module type I_mono = sig
  type t
  include Hashable_intf.Key with type t := t
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
  let cmper = T.{hash_fold; cmp; pp}
end

module type I_poly = sig
  type 'a t
  val hash_fold: ('a -> Hash.State.t -> Hash.State.t) -> 'a t -> Hash.State.t
    -> Hash.State.t
  val hash_fold_a: 'a -> Hash.State.t -> Hash.State.t
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

  let hash_fold t state =
    T.hash_fold T.hash_fold_a t state

  let pp ppf t =
    T.pp T.pp_a ppf t

  let cmper = {hash_fold; cmp=T.cmp; pp}
end
