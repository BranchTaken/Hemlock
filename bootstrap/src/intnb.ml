open Intnb_intf

module type I_common = sig
  include I
  val signed: bool
end

module type S_common = sig
  include S
  val narrow: t -> t
end

module Make_common (T : I_common) : S_common with type t := int = struct
  module U = struct
    type t = int

    let zero = 0

    let cmp t0 t1 =
      let rel = match T.signed || (Sys.int_size > T.num_bits) with
        | true -> compare t0 t1
        | false -> compare (t0 + min_int) (t1 + min_int)
      in
      if rel < 0 then
        Cmp.Lt
      else if rel = 0 then
        Cmp.Eq
      else
        Cmp.Gt

    let narrow t =
      let nlb = Sys.int_size - T.num_bits in
      match nlb with
      | 0 ->  t
      | _ -> begin
          match T.signed with
          | false -> ((1 lsl T.num_bits) - 1) land t
          | true -> (t asr (Sys.int_size - 1) lsl T.num_bits) lor t
        end

    let narrow_of_signed = narrow

    let narrow_of_unsigned t =
      let nlb = Sys.int_size - T.num_bits in
      match nlb with
      | 0 ->  t
      | _ -> ((1 lsl T.num_bits) - 1) land t

    let lbfill t =
      let nlb = Sys.int_size - T.num_bits in
      match nlb with
      | 0 -> t
      | _ -> begin
          let mask = ((1 lsl T.num_bits) - 1) in
          let lb = lnot mask in
          lb lor t
        end

    let lbclear t =
      match T.signed with
      | false -> t
      | true -> begin
          let nlb = Sys.int_size - T.num_bits in
          match nlb with
          | 0 -> begin
              (* Leading bits are already zeros. *)
              assert (narrow t = t);
              t
            end
          | _ -> begin
              let mask = ((1 lsl T.num_bits) - 1) in
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

    let compare t0 t1 =
      assert (narrow t0 = t0);
      assert (narrow t1 = t1);
      match cmp t0 t1 with
      | Cmp.Lt -> -1
      | Cmp.Eq -> 0
      | Cmp.Gt -> 1

    let sexp_of_t t =
      assert (narrow t = t);
      Sexplib.Std.sexp_of_int t

    let t_of_sexp sexp =
      narrow (Sexplib.Std.int_of_sexp sexp)

    let min_value = zero

    let max_value = narrow (0 - 1)

    let one = 1

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
      narrow (t lsl i)

    let bit_usr t i =
      narrow (t lsr i)

    let bit_pop t =
      let x = lbclear t in
      let x = x - (bit_and (bit_usr x 1) 0x5555_5555_5555_5555) in
      let c3s = 0x3333_3333_3333_3333 in
      let x = (bit_and x c3s) + (bit_and (bit_usr x 2) c3s) in
      let x = bit_and (x + (bit_usr x 4)) 0x0f0f_0f0f_0f0f_0f0f in
      let x = x + (bit_usr x 8) in
      let x = x + (bit_usr x 16) in
      let x = x + (bit_usr x 32) in
      bit_and x 0x3f

    let bit_clz t =
      let x = lbclear t in
      let x = bit_or x (bit_usr x 1) in
      let x = bit_or x (bit_usr x 2) in
      let x = bit_or x (bit_usr x 4) in
      let x = bit_or x (bit_usr x 8) in
      let x = bit_or x (bit_usr x 16) in
      let x = bit_or x (bit_usr x 32) in
      let nlb = Sys.int_size - T.num_bits in
      (bit_pop (bit_not x)) - nlb

    let bit_ctz t =
      let t' = lbfill t in
      bit_pop (bit_and (bit_not t') (t' - 1))

    let is_pow2 t =
      assert ((not T.signed) || t >= 0);
      (bit_and t (t - 1)) = 0

    let floor_pow2 t =
      assert ((not T.signed) || t >= 0);
      if t <= 1 then t
      else bit_sl 1 (T.num_bits - 1 - (bit_clz t))

    let ceil_pow2 t =
      assert ((not T.signed) || t >= 0);
      if t <= 1 then t
      else bit_sl 1 (T.num_bits - (bit_clz (t - 1)))

    let floor_lg t =
      assert (t > 0);
      T.num_bits - 1 - (bit_clz t)

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
      let rec lambda r p n = begin
        match n with
        | 0 -> r
        | _ -> begin
            let r' = match bit_and n 1 with
              | 0 -> r
              | 1 -> r * p
              | _ -> assert false
            in
            let p' = p * p in
            let n' = bit_usr n 1 in
            lambda r' p' n'
          end
      end in
      let r = lambda 1 t0 n in
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
  include Cmpable.Make_rel(U)
  include Cmpable.Make_zero(U)
end

module Make_u (T : I) : S_u with type t := int = struct
  module U = struct
    type t = T.t
    let num_bits = T.num_bits
    let signed = false
  end
  include Make_common(U)
end

module Make_i (T : I) : S_i with type t := int = struct
  module U = struct
    type t = T.t
    let num_bits = T.num_bits
    let signed = true
  end
  include Make_common(U)

  let min_value = narrow min_int

  let max_value = narrow max_int

  let neg_one = narrow (-1)

  let bit_ssr t i =
    assert (narrow t = t);
    t asr i

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
