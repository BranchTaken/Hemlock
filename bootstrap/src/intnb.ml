open Intnb_intf

module type I_common = sig
  include I
  val signed: bool
end

module type S_common = sig
  include S
  val narrow: t -> int
end

module Make_common (T : I_common) : S_common with type t := int = struct
  module U = struct
    type t = int

    let hash_fold = Hash.hash_fold

    let narrow t =
      let nlb = Sys.int_size - (int_of_uint T.num_bits) in
      match nlb with
      | 0 ->  t
      | _ -> begin
          match T.signed with
          | false -> ((1 lsl (int_of_uint T.num_bits)) - 1) land t
          | true ->
            (t asr (Sys.int_size - 1) lsl (int_of_uint T.num_bits)) lor t
        end

    let cmp t0 t1 =
      assert (narrow t0 = t0);
      assert (narrow t1 = t1);
      let rel = match T.signed || (Sys.int_size > (int_of_uint T.num_bits)) with
        | true -> compare t0 t1
        | false -> compare (t0 + min_int) (t1 + min_int)
      in
      if rel < 0 then
        Cmp.Lt
      else if rel = 0 then
        Cmp.Eq
      else
        Cmp.Gt

    let narrow_of_signed s =
      match T.signed with
      | true -> narrow s
      | false -> begin
          let nlb = Sys.int_size - (int_of_uint T.num_bits) in
          (match nlb with
            | 0 -> s
            | _ -> ((1 lsl (int_of_uint T.num_bits)) - 1) land s
          )
        end

    let narrow_of_unsigned u =
      match T.signed with
      | true -> begin
          let nlb = Sys.int_size - (int_of_uint T.num_bits) in
          match nlb with
          | 0 -> int_of_uint u
          | _ -> ((1 lsl (int_of_uint T.num_bits)) - 1) land (int_of_uint u)
        end
      | false -> begin
          narrow (int_of_uint u)
        end

    let lbfill t =
      let nlb = Sys.int_size - (int_of_uint T.num_bits) in
      match nlb with
      | 0 -> t
      | _ -> begin
          let mask = ((1 lsl (int_of_uint T.num_bits)) - 1) in
          let lb = lnot mask in
          lb lor t
        end

    let lbclear t =
      match T.signed with
      | false -> t
      | true -> begin
          let nlb = Sys.int_size - (int_of_uint T.num_bits) in
          match nlb with
          | 0 -> begin
              (* Leading bits are already zeros. *)
              assert (narrow t = t);
              t
            end
          | _ -> begin
              let mask = ((1 lsl (int_of_uint T.num_bits)) - 1) in
              t land mask
            end
        end

    let of_float f =
      narrow (int_of_float f)

    let to_float t =
      assert (narrow t = t);
      float_of_int t

    let of_string s =
      narrow (int_of_string s)

    let to_string t =
      assert (narrow t = t);
      string_of_int t

    let pp ppf t =
      assert (narrow t = t);
      Format.fprintf ppf "%d" t

    let pp_x ppf t =
      assert (narrow t = t);
      Format.fprintf ppf "0x%016x" t

    let zero = 0

    let one = 1

    let min_value = narrow min_int

    let max_value = narrow max_int

    let succ t =
      narrow (t + 1)

    let pred t =
      narrow (t - 1)

    let bit_and t0 t1 =
      narrow (t0 land t1)

    let bit_or t0 t1 =
      narrow (t0 lor t1)

    let bit_xor t0 t1 =
      narrow (t0 lxor t1)

    let bit_not t =
      narrow (lnot t)

    let bit_sl t i =
      narrow (t lsl (int_of_uint i))

    let bit_usr t i =
      narrow (t lsr (int_of_uint i))

    let bit_pop t =
      let x = lbclear t in
      let x = x - (bit_and (bit_usr x (uint_of_int 1)) 0x5555_5555_5555_5555) in
      let c3s = 0x3333_3333_3333_3333 in
      let x = (bit_and x c3s) + (bit_and (bit_usr x (uint_of_int 2)) c3s) in
      let x = bit_and (x + (bit_usr x (uint_of_int 4))) 0x0f0f_0f0f_0f0f_0f0f in
      let x = x + (bit_usr x (uint_of_int 8)) in
      let x = x + (bit_usr x (uint_of_int 16)) in
      let x = x + (bit_usr x (uint_of_int 32)) in
      uint_of_int (bit_and x 0x3f)

    let bit_clz t =
      let x = lbclear t in
      let x = bit_or x (bit_usr x (uint_of_int 1)) in
      let x = bit_or x (bit_usr x (uint_of_int 2)) in
      let x = bit_or x (bit_usr x (uint_of_int 4)) in
      let x = bit_or x (bit_usr x (uint_of_int 8)) in
      let x = bit_or x (bit_usr x (uint_of_int 16)) in
      let x = bit_or x (bit_usr x (uint_of_int 32)) in
      bit_pop (bit_not x)

    let bit_ctz t =
      let t' = lbfill t in
      bit_pop (bit_and (bit_not t') (t' - 1))

    let is_pow2 t =
      assert ((not T.signed) || t >= 0);
      (bit_and t (t - 1)) = 0

    let floor_pow2 t =
      assert ((not T.signed) || t >= 0);
      if t <= 1 then t
      else
        bit_sl 1 (uint_of_int
            ((int_of_uint T.num_bits) - 1 - (int_of_uint (bit_clz t))))

    let ceil_pow2 t =
      assert ((not T.signed) || t >= 0);
      if t <= 1 then t
      else bit_sl 1 (uint_of_int
            ((int_of_uint T.num_bits) - (int_of_uint (bit_clz (t - 1)))))

    let floor_lg t =
      assert (t > 0);
      (int_of_uint T.num_bits) - 1 - (int_of_uint (bit_clz t))

    let ceil_lg t =
      assert (t > 0);
      floor_lg t + (if is_pow2 t then 0 else 1)

    let ( + ) t0 t1 =
      narrow (t0 + t1)

    let ( - ) t0 t1 =
      narrow (t0 - t1)

    let ( * ) t0 t1 =
      narrow (t0 * t1)

    let ( / ) t0 t1 =
      narrow (t0 / t1)

    let ( % ) t0 t1 =
      narrow (t0 mod t1)

    let ( ** ) t0 t1 =
      (* Decompose the exponent to limit algorithmic complexity. *)
      let neg, n = if T.signed && t1 < 0 then
          true, -t1
        else
          false, t1
      in
      let rec fn r p n = begin
        match n with
        | 0 -> r
        | _ -> begin
            let r' = match bit_and n 1 with
              | 0 -> r
              | 1 -> r * p
              | _ -> assert false
            in
            let p' = p * p in
            let n' = bit_usr n (uint_of_int 1) in
            fn r' p' n'
          end
      end in
      let r = fn 1 t0 n in
      narrow (
        match neg with
        | false -> r
        | true -> 1 / r
      )

    let ( // ) t0 t1 =
      assert (narrow t0 = t0);
      assert (narrow t1 = t1);
      (to_float t0) /. (to_float t1)

    let min t0 t1 =
      assert (narrow t0 = t0);
      assert (narrow t1 = t1);
      match cmp t0 t1 with
      | Lt | Eq -> t0
      | Gt -> t1

    let max t0 t1 =
      assert (narrow t0 = t0);
      assert (narrow t1 = t1);
      match cmp t0 t1 with
      | Lt | Eq -> t1
      | Gt -> t0
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.Make_zero(U)
end

module Make_i (T : I) : S_i with type t := int = struct
  module U = struct
    type t = int
    let num_bits = T.num_bits
    let signed = true
  end
  include U
  include Make_common(U)

  let neg_one = narrow (-1)

  let bit_ssr t i =
    assert (narrow t = t);
    t asr (int_of_uint i)

  let ( ~- ) t =
    assert (narrow t = t);
    -1 * t

  let ( ~+) t =
    assert (narrow t = t);
    t

  let neg t =
    assert (narrow t = t);
    -t

  let abs t =
    assert (narrow t = t);
    abs t
end

module Make_u (T : I) : S_u with type t := uint = struct
  module U = struct
    module V = struct
      module W = struct
        type t = int
        let num_bits = T.num_bits
        let signed = false
      end
      include W
      include Make_common(W)
    end
    type t = uint

    let hash_fold = Hash.hash_fold

    let cmp t0 t1 =
      V.cmp (int_of_uint t0) (int_of_uint t1)

    let narrow_of_signed x =
      uint_of_int (V.narrow_of_signed x)

    let narrow_of_unsigned x =
      uint_of_int (V.narrow_of_unsigned x)

    let of_float f =
      uint_of_int (V.of_float f)

    let to_float t =
      V.to_float (int_of_uint t)

    let of_string s =
      uint_of_int (V.of_string s)

    let to_string t =
      V.to_string (int_of_uint t)

    let pp ppf t =
      Format.fprintf ppf "%u" (int_of_uint t)

    let pp_x ppf t =
      V.pp_x ppf (int_of_uint t)

    let zero = uint_of_int V.zero

    let one = uint_of_int V.one

    let min_value = zero

    let max_value = uint_of_int (V.narrow (V.zero - V.one))

    let succ t =
      uint_of_int (V.succ (int_of_uint t))

    let pred t =
      uint_of_int (V.pred (int_of_uint t))

    let bit_and t0 t1 =
      uint_of_int (V.bit_and (int_of_uint t0) (int_of_uint t1))

    let bit_or t0 t1 =
      uint_of_int (V.bit_or (int_of_uint t0) (int_of_uint t1))

    let bit_xor t0 t1 =
      uint_of_int (V.bit_xor (int_of_uint t0) (int_of_uint t1))

    let bit_not t =
      uint_of_int (V.bit_not (int_of_uint t))

    let bit_sl t i =
      uint_of_int (V.bit_sl (int_of_uint t) i)

    let bit_usr t i =
      uint_of_int (V.bit_usr (int_of_uint t) i)

    let bit_pop t =
      V.bit_pop (int_of_uint t)

    let bit_clz t =
      V.bit_clz (int_of_uint t)

    let bit_ctz t =
      V.bit_ctz (int_of_uint t)

    let is_pow2 t =
      V.is_pow2 (int_of_uint t)

    let floor_pow2 t =
      uint_of_int (V.floor_pow2 (int_of_uint t))

    let ceil_pow2 t =
      uint_of_int (V.ceil_pow2 (int_of_uint t))

    let floor_lg t =
      uint_of_int (V.floor_lg (int_of_uint t))

    let ceil_lg t =
      uint_of_int (V.ceil_lg (int_of_uint t))

    let ( + ) t0 t1 =
      uint_of_int (V.( + ) (int_of_uint t0) (int_of_uint t1))

    let ( - ) t0 t1 =
      uint_of_int (V.( - ) (int_of_uint t0) (int_of_uint t1))

    let ( * ) t0 t1 =
      uint_of_int (V.( * ) (int_of_uint t0) (int_of_uint t1))

    let ( / ) t0 t1 =
      uint_of_int (V.( / ) (int_of_uint t0) (int_of_uint t1))

    let ( % ) t0 t1 =
      uint_of_int (V.( % ) (int_of_uint t0) (int_of_uint t1))

    let ( ** ) t0 t1 =
      uint_of_int (V.( ** ) (int_of_uint t0) (int_of_uint t1))

    let ( // ) t0 t1 =
      V.( // ) (int_of_uint t0) (int_of_uint t1)

    let min t0 t1 =
      uint_of_int (V.min (int_of_uint t0) (int_of_uint t1))

    let max t0 t1 =
      uint_of_int (V.max (int_of_uint t0) (int_of_uint t1))
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.Make_zero(U)
end
