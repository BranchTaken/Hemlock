(* The redundancy between the .mli and .ml files cannot be extracted into an interface file,
 * apparently because the phantom witness type somehow causes two incompatible-but-identical
 * comparator types to exist. *)

type ('a, 'witness) t = {
  hash_fold: 'a -> Hash.State.t -> Hash.State.t;
  cmp: 'a -> 'a -> Cmp.t;
  pp: 'a -> (module Fmt.Formatter) -> (module Fmt.Formatter)
}

type ('a, 'witness) cmper = ('a, 'witness) t

module type IMono = sig
  type t
  include CmpableIntf.Key with type t := t
  include FormattableIntf.SMono with type t := t
end

module type SMono = sig
  type t
  type cmper_witness
  val cmper: (t, cmper_witness) cmper
end

module MakeMono (T : IMono) : SMono with type t := T.t = struct
  type cmper_witness
  let cmper = T.{hash_fold; cmp; pp}
end

module type IPoly = sig
  type 'a t
  val hash_fold: ('a -> Hash.State.t -> Hash.State.t) -> 'a t -> Hash.State.t
    -> Hash.State.t
  val hash_fold_a: 'a -> Hash.State.t -> Hash.State.t
  include CmpableIntf.IPoly with type 'a t := 'a t
  include FormattableIntf.SPoly with type 'a t := 'a t
  val pp_a: 'a -> (module Fmt.Formatter) -> (module Fmt.Formatter)
end

module type SPoly = sig
  type 'a t
  type cmper_witness
  val cmper: ('a t, cmper_witness) cmper
end

module MakePoly (T : IPoly) : SPoly with type 'a t := 'a T.t = struct
  type cmper_witness

  let hash_fold t state =
    T.hash_fold T.hash_fold_a t state

  let pp t formatter =
    formatter |> T.(pp pp_a) t

  let cmper = {hash_fold; cmp=T.cmp; pp}
end
