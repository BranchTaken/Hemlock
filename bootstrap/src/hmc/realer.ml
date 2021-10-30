open Basis
open Basis.Rudiments

module T = struct
  type sign =
    | Neg
    | Pos
  type magnitude =
    | Fin of {
      exponent: Zint.t;
      mantissa: Nat.t;
    }
    | Inf
  type t =
    | N of {
      sign: sign;
      mag: magnitude;
    }
    | Nan

  let zero = N {sign=Pos; mag=Fin {exponent=Zint.zero; mantissa=Nat.zero}}

  let is_fin = function
    | N {sign=_; mag=(Fin _)} -> true
    | N {sign=_; mag=Inf} -> false
    | Nan -> false

  let is_inf = function
    | N {sign=_; mag=(Fin _)} -> false
    | N {sign=_; mag=Inf} -> true
    | Nan -> false

  let is_nan = function
    | N _ -> false
    | Nan -> true

  let is_norm = function
    | N {sign=_; mag=Fin {exponent; mantissa}} -> begin
        Nat.bit_ctz mantissa = 0L && (Nat.(mantissa <> zero) || Zint.(exponent = zero))
      end
    | N {sign=_; mag=Inf} -> true
    | Nan -> true

  (* Remove trailing zeros from mantissa, set exponent to 0 for zeros. *)
  let norm t =
    match t with
    | N {sign; mag=Fin {exponent; mantissa}} -> begin
        match Nat.bit_ctz mantissa, Nat.(mantissa = zero) with
        | 0L, false -> t
        | tz, false -> N {sign; mag=Fin {exponent; mantissa=Nat.(bit_usr ~shift:tz mantissa)}}
        | _, true -> N {sign; mag=Fin {exponent=Zint.zero; mantissa=Nat.zero}}
      end
    | N {sign=_; mag=Inf} -> t
    | Nan -> t

  (* Denormalize the inputs such that their binary points are aligned, by padding at most one of the
   * mantissas with trailing 0 digits. This enables digit-aligned operations (comparison, addition,
   * subtraction). *)
  let denorm t0 t1 =
    match t0, t1 with
    | N {sign=s0; mag=Fin {exponent=e0; mantissa=m0}},
      N {sign=s1; mag=Fin {exponent=e1; mantissa=m1}} -> begin
        assert (is_norm t0);
        assert (is_norm t1);
        match Nat.(m0 = zero) || Nat.(m1 = zero) with
        | true -> t0, t1
        | false -> begin
            let frac_ndigits0 = pred ((Nat.bit_length m0) - (Nat.bit_clz m0)) in
            let frac_ndigits1 = pred ((Nat.bit_length m1) - (Nat.bit_clz m1)) in
            let min_fixed_frac_ndigits0 = Zint.(of_uns frac_ndigits0 - e0) in
            let min_fixed_frac_ndigits1 = Zint.(of_uns frac_ndigits1 - e1) in
            match Zint.cmp min_fixed_frac_ndigits0 min_fixed_frac_ndigits1 with
            | Lt -> begin
                let shift = Zint.(to_uns_hlt (min_fixed_frac_ndigits1 - min_fixed_frac_ndigits0)) in
                N {sign=s0; mag=Fin {exponent=e0; mantissa=Nat.bit_sl ~shift m0}}, t1
              end
            | Eq -> t0, t1
            | Gt -> begin
                let shift = Zint.(to_uns_hlt (min_fixed_frac_ndigits0 - min_fixed_frac_ndigits1)) in
                t0, N {sign=s1; mag=Fin {exponent=e1; mantissa=Nat.bit_sl ~shift m1}}
              end
          end
      end
    | N {sign=_; mag=Inf}, _
    | _, N {sign=_; mag=Inf}
    | Nan, _
    | _, Nan -> t0, t1

  let hash_fold t state =
    assert (is_norm t);
    let open Hash.State.Gen in
    let hash_fold_sign t state = begin
      state
      |> Hash.State.Gen.fold_u64 1L ~f:(fun _ ->
        match t with
        | Neg -> 0L
        | Pos -> 2L
      )
    end in
    match t with
    | N {sign; mag=Fin {exponent; mantissa}} -> begin
        init state
        |> fold_u64 1L ~f:(fun _ -> 0L)
        |> hash_fold_sign sign
        |> fini
        |> Zint.hash_fold exponent
        |> Nat.hash_fold mantissa
      end
    | N {sign; mag=Inf} -> begin
        init state
        |> fold_u64 1L ~f:(fun _ -> 1L)
        |> hash_fold_sign sign
        |> fini
      end
    | Nan -> begin
        init state
        |> fold_u64 1L ~f:(fun _ -> 2L)
        |> fini
      end

  let cmp t0 t1 =
    assert (is_norm t0);
    assert (is_norm t1);
    let open Cmp in
    match t0, t1 with
    | (N {sign=Pos; mag=(Fin {exponent=e0; mantissa=_})} as n0),
      (N {sign=Pos; mag=(Fin {exponent=e1; mantissa=_})} as n1)
    | (N {sign=Neg; mag=(Fin {exponent=e1; mantissa=_})} as n1),
      (N {sign=Neg; mag=(Fin {exponent=e0; mantissa=_})} as n0) -> begin
        (* If exponents are unequal, the larger exponent corresponds to the * larger/smaller value,
         * for Pos/Neg respectively. *)
        match Zint.cmp e0 e1 with
        | Lt -> Lt
        | Eq -> begin
            (* Denormalize, such that the values have the same number of * significant binary
             * digits. This requires padding one of the * mantissas with trailing zeros. *)
            match denorm n0 n1 with
            | N {sign=_; mag=(Fin {exponent=_; mantissa=m0})},
              N {sign=_; mag=(Fin {exponent=_; mantissa=m1})} -> Nat.cmp m0 m1
            | _ -> not_reached ()
          end
        | Gt -> Gt
      end

    | N {sign=Neg; mag=Fin {exponent=_; mantissa=m0}},
      N {sign=Pos; mag=Fin {exponent=_; mantissa=m1}} -> begin
        match Nat.(m0 = zero) && Nat.(m1 = zero) with
        | true -> Eq
        | false -> Lt
      end
    | N {sign=Pos; mag=Fin {exponent=_; mantissa=m0}},
      N {sign=Neg; mag=Fin {exponent=_; mantissa=m1}} -> begin
        match Nat.(m0 = zero) && Nat.(m1 = zero) with
        | true -> Eq
        | false -> Gt
      end

    | N {sign=Neg; mag=Inf}, N {sign=Pos; mag=Inf} -> Lt
    | N {sign=Neg; mag=Inf}, N {sign=Neg; mag=Inf}
    | N {sign=Pos; mag=Inf}, N {sign=Pos; mag=Inf} -> Eq
    | N {sign=Pos; mag=Inf}, N {sign=Neg; mag=Inf} -> Gt

    | N {sign=Neg; mag=Inf}, N {sign=_; mag=(Fin _)} -> Lt
    | N {sign=Pos; mag=Inf}, N {sign=_; mag=(Fin _)} -> Gt

    | N {sign=_; mag=(Fin _)}, N {sign=Neg; mag=Inf} -> Gt
    | N {sign=_; mag=(Fin _)}, N {sign=Pos; mag=Inf} -> Lt

    | Nan, _
    | _, Nan -> halt "Comparison with nan"

  let pp ppf t =
    assert (is_norm t);
    let open Format in
    let pp_sign ppf = function
      | Neg -> fprintf ppf "-"
      | Pos -> fprintf ppf ""
    in
    let rec pp_frac hex_digits ppf frac = begin
      match hex_digits with
      | 0L -> ()
      | _ -> begin
          let digit = Nat.(bit_and frac k_f) in
          let int64_digit = match Nat.(digit = zero) with
            | true -> Int64.zero
            | false -> Nat.get 0L digit
          in
          let frac' = Nat.bit_usr ~shift:4L frac in
          pp_frac (pred hex_digits) ppf frac';
          fprintf ppf "%Lx" int64_digit
        end
    end in
    match t with
    | N {sign; mag=Fin {exponent; mantissa}} -> begin
        match Nat.(mantissa = zero) with
        | true -> fprintf ppf "%a0x0p0" pp_sign sign
        | false -> begin
            let sig_digits = Uns.( - ) (Nat.bit_length mantissa) (Nat.bit_clz mantissa) in
            let shift = pred sig_digits in
            let frac_mask = Nat.((bit_sl ~shift one) - one) in
            let frac = Nat.bit_and mantissa frac_mask in
            match Nat.(frac = zero) with
            | true -> fprintf ppf "%a0x1p%a" pp_sign sign Zint.pp exponent
            | false -> begin
                let rem = shift % 4L in
                let hex_digits = shift / 4L in
                let hex_digits', frac' = match rem = 0L with
                  | true -> hex_digits, frac
                  | false -> (succ hex_digits), Nat.bit_sl ~shift:(4L - rem) frac
                in
                fprintf ppf "%a0x1.%ap%a"
                  pp_sign sign
                  (pp_frac hex_digits') frac'
                  Zint.pp exponent
              end
          end
      end
    | N {sign; mag=Inf} -> fprintf ppf "%ainf" pp_sign sign
    | Nan -> fprintf ppf "nan"
end
include T
include Identifiable.Make(T)
include Cmpable.MakeZero(T)

let create ~sign ~exponent ~mantissa =
  norm (N {sign; mag=Fin {exponent; mantissa}})

let one = N {sign=Pos; mag=Fin {exponent=Zint.zero; mantissa=Nat.one}}

let inf = N {sign=Pos; mag=Inf}

let nan = Nan

let ( ~- ) = function
  | N {sign=Neg; mag} -> N {sign=Pos; mag}
  | N {sign=Pos; mag} -> N {sign=Neg; mag}
  | Nan -> Nan

let ( ~+ ) t =
  t

let neg t =
  ~-t

let abs = function
  | N {sign=_; mag} -> N {sign=Pos; mag}
  | Nan -> Nan

type add_sub_op =
  | Add
  | Sub

let ( + ) t0 t1 =
  assert (is_norm t0);
  assert (is_norm t1);
  let add_sub op t0 t1 = begin
    match denorm t0 t1 with
    | N {sign; mag=Fin {exponent=e0; mantissa=m0}},
      N {sign=_; mag=Fin {exponent=e1; mantissa=m1}} -> begin
        let mantissa_ndigits0 = (Nat.bit_length m0) - (Nat.bit_clz m0) in
        let mantissa_ndigits1 = (Nat.bit_length m1) - (Nat.bit_clz m1) in
        let mantissa' = match op with
          | Add -> Nat.(m0 + m1)
          | Sub -> Nat.(m0 - m1)
        in
        let mantissa'_ndigits = Zint.of_uns
            ((Nat.bit_length mantissa') - (Nat.bit_clz mantissa')) in
        let exponent' = match Uns.cmp mantissa_ndigits0 mantissa_ndigits1 with
          | Lt -> Zint.(e1 + mantissa'_ndigits - (of_uns mantissa_ndigits1))
          | Eq
          | Gt -> Zint.(e0 + mantissa'_ndigits - (of_uns mantissa_ndigits0))
        in
        norm (N {sign; mag=Fin {exponent=exponent'; mantissa=mantissa'}})
      end
    | N {sign=_; mag=Inf}, _
    | _, N {sign=_; mag=Inf}
    | Nan, _
    | _, Nan -> not_reached ()
  end in
  match t0, t1 with
  | N {sign=s0; mag=Fin {exponent=_; mantissa=m0}},
    N {sign=s1; mag=Fin {exponent=_; mantissa=m1}} -> begin
      match Nat.(m0 = zero), Nat.(m1 = zero) with
      | true, true -> begin
          match s0, s1 with
          | Pos, Pos
          | Pos, Neg -> t0
          | Neg, Pos
          | Neg, Neg -> t1
        end
      | false, true -> t0
      | true, false -> t1
      | false, false -> begin
          match s0, s1 with
          | Pos, Pos
          | Neg, Neg -> add_sub Add t0 t1
          | Pos, Neg -> add_sub Sub t0 t1
          | Neg, Pos -> add_sub Sub t1 t0
        end
    end

  | N {sign=_; mag=Inf}, N {sign=_; mag=Fin _} -> t0
  | N {sign=_; mag=Fin _}, N {sign=_; mag=Inf} -> t1

  | N {sign=Pos; mag=Inf}, N {sign=Pos; mag=Inf} -> t0
  | N {sign=Neg; mag=Inf}, N {sign=Neg; mag=Inf} -> t0

  | N {sign=Pos; mag=Inf}, N {sign=Neg; mag=Inf}
  | N {sign=Neg; mag=Inf}, N {sign=Pos; mag=Inf}
  | Nan, _
  | _, Nan -> Nan

let ( - ) t0 t1 =
  assert (is_norm t0);
  assert (is_norm t1);
  t0 + (neg t1)

let sign_mul s0 s1 =
  match s0, s1 with
  | Pos, Pos
  | Neg, Neg -> Pos
  | Pos, Neg
  | Neg, Pos -> Neg

let ( * ) t0 t1 =
  assert (is_norm t0);
  assert (is_norm t1);
  match t0, t1 with
  | N {sign=s0; mag=Fin {exponent=e0; mantissa=m0}},
    N {sign=s1; mag=Fin {exponent=e1; mantissa=m1}} -> begin
      norm (N {sign=sign_mul s0 s1; mag=Fin {exponent=Zint.(e0 + e1); mantissa=Nat.(m0 * m1)}})
    end

  | N {sign=s0; mag=Inf}, N {sign=s1; mag=Fin {exponent=_; mantissa}}
  | N {sign=s0; mag=Fin {exponent=_; mantissa}}, N {sign=s1; mag=Inf} -> begin
      match Nat.(mantissa = zero) with
      | true -> nan
      | false -> N {sign=sign_mul s0 s1; mag=Inf}
    end

  | N {sign=_; mag=Inf}, N {sign=Pos; mag=Inf} -> t0
  | N {sign=Pos; mag=Inf}, N {sign=Neg; mag=Inf} -> t1
  | N {sign=Neg; mag=Inf}, N {sign=Neg; mag=Inf} -> inf

  | Nan, _
  | _, Nan -> Nan

type precision =
  | Precise
  | Rounded

type mbits =
  | Mbits_53
  | Mbits_24

let to_r_impl emin emax mbits t =
  assert (is_norm t);
  match t with
  | N {sign; mag=Fin {exponent; mantissa}} -> begin
      match Zint.(exponent < emin) with
      | true -> begin
          match sign with
          | Pos -> Rounded, 0.
          | Neg -> Rounded, Real.(neg 0.)
        end
      | false -> begin
          match Zint.(exponent > emax) with
          | true -> begin
              match sign with
              | Pos -> Rounded, Real.inf
              | Neg -> Rounded, Real.neg_inf
            end
          | false -> begin
              let e = Zint.to_sint exponent in
              match Nat.(mantissa = zero) with
              | true -> begin
                  Precise, (match sign with
                    | Pos -> 0.
                    | Neg -> -0.
                  )
                end
              | false -> begin
                  let neg = match sign with
                    | Neg -> true
                    | Pos -> false
                  in
                  let sig_digits = Uns.( - ) (Nat.bit_length mantissa) (Nat.bit_clz mantissa) in
                  let max_mbits, mask = match mbits with
                    | Mbits_53 -> 53L, 0xf_ffff_ffff_ffffL
                    | Mbits_24 -> 24L, 0xf_ffff_e000_0000L
                  in
                  let precision = match Uns.(sig_digits <= max_mbits) with
                    | true -> Precise
                    | false -> Rounded
                  in
                  let m = Uns.bit_and mask (match Uns.cmp sig_digits 53L with
                    | Lt -> begin
                        let shift = Uns.(53L - sig_digits) in
                        Nat.(to_uns (bit_sl ~shift mantissa))
                      end
                    | Eq -> Nat.to_uns mantissa
                    | Gt -> begin
                        let shift = Uns.(sig_digits - 53L) in
                        Nat.(to_uns (bit_usr ~shift mantissa))
                      end
                  ) in
                  precision, Real.create ~neg ~exponent:e ~mantissa:m
                end
            end
        end
    end

  | N {sign=Pos; mag=Inf} -> Precise, Real.inf
  | N {sign=Neg; mag=Inf} -> Precise, Real.neg_inf

  | Nan -> Precise, Real.nan

let z_1023 = Zint.(pred (bit_sl ~shift:10L one))
let z_neg_1022 = Zint.(neg (pred z_1023))

let to_r64_impl t =
  to_r_impl z_neg_1022 z_1023 Mbits_53 t

let to_r64 t =
  match to_r64_impl t with _precision, r64 -> r64

let to_r64_opt t =
  match to_r64_impl t with
  | Precise, r64 -> Some r64
  | Rounded, _ -> None

let to_r64_hlt t =
  match to_r64_impl t with
  | Precise, r64 -> r64
  | Rounded, _ -> halt "Inexact conversion"

let z_127 = Zint.(pred (bit_sl ~shift:7L one))
let z_neg_126 = Zint.(neg (pred z_127))

let to_r32_impl t =
  to_r_impl z_neg_126 z_127 Mbits_24 t

let to_r32 t =
  match to_r32_impl t with _precision, r32 -> r32

let to_r32_opt t =
  match to_r32_impl t with
  | Precise, r32 -> Some r32
  | Rounded, _ -> None

let to_r32_hlt t =
  match to_r32_impl t with
  | Precise, r32 -> r32
  | Rounded, _ -> halt "Inexact conversion"
