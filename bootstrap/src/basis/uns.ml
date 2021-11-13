open RudimentsInt0
open RudimentsFunctions

module T = struct
  module U = struct
    type t = int64

    let hash_fold t state =
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u64 1L ~f:(fun _ -> t)
      |> Hash.State.Gen.fini

    let cmp t0 t1 =
      let open Cmp in
      let rel = Int64.unsigned_compare t0 t1 in
      if Stdlib.(rel < 0) then Lt
      else if Stdlib.(rel = 0) then Eq
      else Gt

    let zero = Int64.zero

    let one = Int64.one

    let xpp xppf t =
      Format.fprintf xppf "%Lu" t

    let to_string ?(sign=Fmt.sign_default) ?(alt=Fmt.alt_default) ?(zpad=Fmt.zpad_default)
      ?(width=Fmt.width_default) ?(base=Fmt.base_default) ?(pretty=Fmt.pretty_default) t =
      let rec fn accum ndigits t = begin
        match Stdlib.(Int64.(unsigned_compare t 0L) = 0)
              && Stdlib.(not zpad || (ndigits >= (Int64.to_int width))) with
        | true -> begin
            (match sign with Implicit -> "" | Explicit -> "+" | Space -> " ")
            ^ (match alt with
              | true -> begin
                  match base with
                  | Bin -> "0b"
                  | Oct -> "0o"
                  | Dec -> ""
                  | Hex -> "0x"
                end
              | false -> ""
            )
            ^ (Stdlib.String.concat "" (match ndigits with 0 -> ["0"] | _ -> accum))
            ^ (match pretty with false -> "" | true -> "u")
          end
        | _ -> begin
            let divisor, group = match base with
              | Bin -> 2L, 8
              | Oct -> 8L, 3
              | Dec -> 10L, 3
              | Hex -> 16L, 4
            in
            let sep = match alt && Stdlib.(ndigits > 0) && Stdlib.((ndigits mod group) = 0) with
              | true -> ["_"]
              | false -> []
            in
            let digit = Stdlib.String.init 1 (fun _ ->
              (Stdlib.String.get "0123456789abcdef" Int64.(to_int (unsigned_rem t divisor)))) in
            let t' = Int64.unsigned_div t divisor in
            fn (digit :: (sep @ accum)) Stdlib.(succ ndigits) t'
          end
      end in
      fn [] 0 t

    let fmt ?pad ?just ?sign ?alt ?zpad ?width ?base ?pretty t formatter =
      Fmt.fmt ?pad ?just ?width (to_string ?sign ?alt ?zpad ?width ?base ?pretty t) formatter

    let pp t formatter =
      fmt ~alt:true t formatter
  end
  include U
  include Cmpable.MakeZero(U)
  include Identifiable.Make(U)

  let xpp_b xppf t =
    let rec fn x shift = begin
      match shift with
      | 0 -> ()
      | _ -> begin
          if Stdlib.(shift mod 8 = 0 && shift < 64) then Format.fprintf xppf "_";
          let shift' = Stdlib.(pred shift) in
          let bit = Int64.(logand (shift_right_logical x shift') 0x1L) in
          Format.fprintf xppf "%Ld" bit;
          fn x shift'
        end
    end in
    Format.fprintf xppf "0b";
    fn t 64

  let xpp_o xppf t =
    Format.fprintf xppf "0o%Lo" t

  let xpp_x xppf t =
    let rec fn x shift = begin
      match shift with
      | 0 -> ()
      | _ -> begin
          if Stdlib.(shift < 64) then Format.fprintf xppf "_";
          let shift' = Stdlib.(shift - 16) in
          Format.fprintf xppf "%04Lx" Int64.(logand (shift_right_logical x shift') 0xffffL);
          fn x shift'
        end
    end in
    Format.fprintf xppf "0x";
    fn t 64

  let of_string s =
    match Stdlib.String.split_on_char 'x' s with
    | "0" :: _ -> Int64.of_string s (* Has 0x prefix. *)
    | _ -> begin
        (* Prefix with "0u" so that the string is interpreted as unsigned. *)
        Int64.of_string ("0u" ^ s)
      end

  let cf_ffff_ffff_ffff = 0xf_ffff_ffff_ffffL
  let c7ff = 0x7ffL
  let c10_0000_0000_0000 = 0x10_0000_0000_0000L

  let of_real r =
    match Float.classify_float r with
    | FP_normal -> begin
        match Stdlib.(Float.(compare r 0.) >= 0) with
        | false -> zero
        | true -> begin
            let bits = Int64.bits_of_float r in
            let biased_exponent = Int64.(to_int (logand (shift_right_logical bits 52) c7ff)) in
            match Stdlib.(biased_exponent >= 1023) with
            | false -> zero
            | true -> begin
                let exponent = Stdlib.(biased_exponent - 1023) in
                let significand = Int64.(logor c10_0000_0000_0000 (logand bits cf_ffff_ffff_ffff)) in
                if Stdlib.(exponent < 52) then
                  Int64.shift_right_logical significand Stdlib.(52 - exponent)
                else if Stdlib.(exponent < 116) then
                  Int64.shift_left significand Stdlib.(exponent - 52)
                else
                  zero
              end
          end
      end
    | FP_subnormal
    | FP_zero
    | FP_infinite -> zero
    | FP_nan -> halt "Not a number"

  let c8000_0000_0000_0000 = of_string "0x8000_0000_0000_0000"
  let c43e0_0000_0000_0000 = of_string "0x43e0_0000_0000_0000"

  let to_real t =
    match Stdlib.(Int64.(compare (logand t c8000_0000_0000_0000) zero) = 0)
    with
    | true -> Int64.to_float t
    | false -> begin
        let fraction = Int64.(logand (shift_right_logical t 11) cf_ffff_ffff_ffff) in
        let exponent = c43e0_0000_0000_0000 in
        let bits = Int64.logor exponent fraction in
        Int64.float_of_bits bits
      end

  let bits_of_i64 x =
    x

  let bits_to_i64 t =
    t

  let like_of_i64_opt x =
    match Stdlib.(Int64.(compare x 0L) < 0) with
    | true -> None
    | false -> Some x

  let like_to_i64_opt t =
    match t > 0x8000_0000_0000_0000L with
    | true -> None
    | false -> Some t

  let like_of_i64_hlt x =
    match like_of_i64_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let like_to_i64_hlt t =
    match like_to_i64_opt t with
    | None -> halt "Lossy conversion"
    | Some x -> x

  let bits_of_sint = bits_of_i64
  let bits_to_sint = bits_to_i64
  let like_of_sint_opt = like_of_i64_opt
  let like_to_sint_opt = like_to_i64_opt
  let like_of_sint_hlt = like_of_i64_hlt
  let like_to_sint_hlt = like_to_i64_hlt

  let extend_of_int x =
    Int64.of_int x

  let trunc_to_int t =
    Int64.to_int t

  let narrow_to_int_opt t =
    match Stdlib.(t > (Int64.of_int Int.max_int)) with
    | true -> None
    | false -> Some (Int64.to_int t)

  let narrow_to_int_hlt t =
    match narrow_to_int_opt t with
    | None -> halt "Lossy conversion"
    | Some x -> x

  let min_value = zero

  let max_value = Int64.minus_one

  let succ t =
    Int64.add t one

  let pred t =
    Int64.sub t one

  let bit_and t0 t1 =
    Int64.logand t0 t1

  let bit_or t0 t1 =
    Int64.logor t0 t1

  let bit_xor t0 t1 =
    Int64.logxor t0 t1

  let bit_not t =
    Int64.lognot t

  let bit_sl ~shift t =
    Int64.(shift_left t (to_int shift))

  let bit_usr ~shift t =
    Int64.(shift_right_logical t (to_int shift))

  let bit_ssr ~shift t =
    Int64.(shift_right t (to_int shift))

  let ( + ) t0 t1 =
    Int64.add t0 t1

  let ( - ) t0 t1 =
    Int64.sub t0 t1

  let ( * ) t0 t1 =
    Int64.mul t0 t1

  let ( / ) t0 t1 =
    Int64.unsigned_div t0 t1

  let ( % ) t0 t1 =
    Int64.unsigned_rem t0 t1

  let ( ** ) t0 t1 =
    (* Decompose the exponent to limit algorithmic complexity. *)
    let rec fn r p n = begin
      match n = zero with
      | true -> r
      | false -> begin
          let r' =
            match (bit_and n one) = zero with
            | true -> r
            | false -> r * p
          in
          let p' = p * p in
          let n' = bit_usr ~shift:1L n in
          fn r' p' n'
        end
    end in
    fn one t0 t1

  let ( // ) t0 t1 =
    (to_real t0) /. (to_real t1)

  external bit_pop: t -> uns = "hemlock_u64_bit_pop"
  external bit_clz: t -> uns = "hemlock_u64_bit_clz"
  external bit_ctz: t -> uns = "hemlock_u64_bit_ctz"

  module V = struct
    type nonrec t = t

    let bit_length = 64L

    let cmp = cmp
    let zero = zero
    let one = one
    let of_uns t = t
    let ( + ) = ( + )
    let ( - ) = ( - )
    let bit_and = bit_and
    let bit_sl = bit_sl
    let bit_clz = bit_clz
  end
  include Intnb.MakeDerived(V)

  let narrow_of_signed x =
    x

  let narrow_of_unsigned u =
    u

  let widen t =
    t
end
include T

module UX512 = Convert.Make_nbU_wX(T)(I512)
let trunc_of_i512 = UX512.trunc_of_x
let extend_to_i512 = UX512.extend_to_x
let narrow_of_i512_opt = UX512.narrow_of_x_opt
let narrow_of_i512_hlt = UX512.narrow_of_x_hlt

module UU512 = Convert.Make_nbU_wU(T)(U512)
let trunc_of_u512 = UU512.trunc_of_u
let extend_to_u512 = UU512.extend_to_u
let narrow_of_u512_opt = UU512.narrow_of_u_opt
let narrow_of_u512_hlt = UU512.narrow_of_u_hlt

module UX256 = Convert.Make_nbU_wX(T)(I256)
let trunc_of_i256 = UX256.trunc_of_x
let extend_to_i256 = UX256.extend_to_x
let narrow_of_i256_opt = UX256.narrow_of_x_opt
let narrow_of_i256_hlt = UX256.narrow_of_x_hlt

module UU256 = Convert.Make_nbU_wU(T)(U256)
let trunc_of_u256 = UU256.trunc_of_u
let extend_to_u256 = UU256.extend_to_u
let narrow_of_u256_opt = UU256.narrow_of_u_opt
let narrow_of_u256_hlt = UU256.narrow_of_u_hlt

module UX128 = Convert.Make_nbU_wX(T)(I128)
let trunc_of_i128 = UX128.trunc_of_x
let extend_to_i128 = UX128.extend_to_x
let narrow_of_i128_opt = UX128.narrow_of_x_opt
let narrow_of_i128_hlt = UX128.narrow_of_x_hlt

module UU128 = Convert.Make_nbU_wU(T)(U128)
let trunc_of_u128 = UU128.trunc_of_u
let extend_to_u128 = UU128.extend_to_u
let narrow_of_u128_opt = UU128.narrow_of_u_opt
let narrow_of_u128_hlt = UU128.narrow_of_u_hlt
