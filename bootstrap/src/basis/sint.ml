open RudimentsInt0
open RudimentsFunctions

module T = struct
  module U = struct
    type t = uns

    let hash_fold t state =
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u64 1L ~f:(fun _ -> t)
      |> Hash.State.Gen.fini

    let cmp t0 t1 =
      let open Cmp in
      let rel = Int64.compare t0 t1 in
      if Stdlib.(rel < 0) then Lt
      else if Stdlib.(rel = 0) then Eq
      else Gt

    let zero = Int64.zero

    let one = Int64.one

    let to_string ?(sign=Fmt.sign_default) ?(alt=Fmt.alt_default) ?(zpad=Fmt.zpad_default)
      ?(width=Fmt.width_default) ?(radix=Fmt.radix_default) ?(pretty=Fmt.pretty_default) t =
      let rec fn accum ndigits is_neg t = begin
        match Stdlib.(Int64.(unsigned_compare t 0L) = 0)
              && Stdlib.(not zpad || (ndigits >= (Int64.to_int width))) with
        | true -> begin
            (match sign, is_neg with
              | Implicit, false -> ""
              | Explicit, false -> "+"
              | Space, false -> " "
              | _, true -> "-"
            )
            ^ (match alt with
              | true -> begin
                  match radix with
                  | Bin -> "0b"
                  | Oct -> "0o"
                  | Dec -> ""
                  | Hex -> "0x"
                end
              | false -> ""
            )
            ^ (Stdlib.String.concat "" (match ndigits with 0 -> ["0"] | _ -> accum))
            ^ (match pretty with false -> "" | true -> "i")
          end
        | _ -> begin
            let divisor, group = match radix with
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
            fn (digit :: (sep @ accum)) Stdlib.(succ ndigits) is_neg t'
          end
      end in
      match Stdlib.((Int64.compare t 0L) < 0) with
      | false -> fn [] 0 false t
      | true -> fn [] 0 true (Int64.neg t)

    let fmt ?pad ?just ?sign ?alt ?zpad ?width ?radix ?pretty t formatter =
      Fmt.fmt ?pad ?just ?width (to_string ?sign ?alt ?zpad ?width ?radix ?pretty t) formatter

    let pp t formatter =
      fmt ~alt:true ~pretty:true t formatter
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.MakeZero(U)

  let of_string s =
    Int64.of_string s

  let of_real r =
    match Float.classify_float r with
    | FP_normal -> begin
        match Stdlib.(Float.(compare r (-0x1p63)) <= 0), Stdlib.(Float.(compare r 0x1p63) >= 0) with
        | true, _ -> Int64.min_int
        | false, false -> Int64.of_float r
        | _, true -> Int64.max_int
      end
    | FP_subnormal
    | FP_zero
    | FP_infinite -> zero
    | FP_nan -> halt "Not a number"

  let to_real t =
    Int64.to_float t

  let of_uns u =
    u

  let min_value = Int64.min_int

  let max_value = Int64.max_int

  let succ = U64.succ

  let pred = U64.pred

  let bit_and = U64.bit_and

  let bit_or = U64.bit_or

  let bit_xor = U64.bit_xor

  let bit_not = U64.bit_not

  let bit_sl = U64.bit_sl

  let bit_sr ~shift t =
    Int64.(shift_right t (to_int shift))

  let ( + ) = U64.( + )

  let ( - ) = U64.( - )

  let ( * ) = U64.( * )

  let ( / ) = U64.( / )

  let ( % ) = U64.( % )

  let ( ** ) t0 t1 =
    (* Decompose the exponent to limit algorithmic complexity. *)
    let neg, n = if t1 < 0L then
        true, Int64.neg t1
      else
        false, t1
    in
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
          let n' = bit_sr ~shift:1L n in
          fn r' p' n'
        end
    end in
    let r = fn 1L t0 n in
    match neg with
    | false -> r
    | true -> one / r

  let ( // ) t0 t1 =
    (to_real t0) /. (to_real t1)

  let bit_pop = U64.bit_pop

  let bit_clz = U64.bit_clz

  let bit_ctz = U64.bit_ctz

  module V = struct
    type nonrec t = t

    let bit_length = 64L

    let cmp = cmp
    let zero = zero
    let one = one
    let of_uns = of_uns
    let ( + ) = ( + )
    let ( - ) = ( - )
    let bit_and = bit_and
    let bit_sl = bit_sl
    let bit_clz = bit_clz
  end
  include Intnb.MakeDerived(V)

  let abs t =
    Int64.abs t

  let neg t =
    Int64.neg t

  let ( ~+ ) t =
    t

  let ( ~- ) t =
    neg t

  let neg_one = Int64.minus_one

  let kv k =
    k

  let extend_of_int x =
    Int64.of_int x

  let trunc_to_int t =
    Int64.to_int t

  let narrow_to_int_opt t =
    match Stdlib.(t < (Int64.of_int Int.min_int) || t > (Int64.of_int Int.max_int)) with
    | true -> None
    | false -> Some (Int64.to_int t)

  let narrow_to_int_hlt t =
    match narrow_to_int_opt t with
    | None -> halt "Lossy conversion"
    | Some x -> x

  let narrow_of_signed x =
    x

  let narrow_of_unsigned u =
    u

  let widen t =
    t
end
include T

module IZ = Convert.Make_nbI_wZ(T)(Zint)
let trunc_of_zint = IZ.trunc_of_x
let extend_to_zint = IZ.extend_to_x
let narrow_of_zint_opt = IZ.narrow_of_x_opt
let narrow_of_zint_hlt = IZ.narrow_of_x_hlt

module IN = Convert.Make_nbI_wN(T)(Nat)
let trunc_of_nat = IN.trunc_of_u
let narrow_of_nat_opt = IN.narrow_of_u_opt
let widen_to_nat_opt = IN.widen_to_u_opt
let narrow_of_nat_hlt = IN.narrow_of_u_hlt
let widen_to_nat_hlt = IN.widen_to_u_hlt

module IX512 = Convert.Make_nbI_wX(T)(I512)
let trunc_of_i512 = IX512.trunc_of_x
let extend_to_i512 = IX512.extend_to_x
let narrow_of_i512_opt = IX512.narrow_of_x_opt
let narrow_of_i512_hlt = IX512.narrow_of_x_hlt

module IU512 = Convert.Make_nbI_wU(T)(U512)
let trunc_of_u512 = IU512.trunc_of_u
let narrow_of_u512_opt = IU512.narrow_of_u_opt
let widen_to_u512_opt = IU512.widen_to_u_opt
let narrow_of_u512_hlt = IU512.narrow_of_u_hlt
let widen_to_u512_hlt = IU512.widen_to_u_hlt

module IX256 = Convert.Make_nbI_wX(T)(I256)
let trunc_of_i256 = IX256.trunc_of_x
let extend_to_i256 = IX256.extend_to_x
let narrow_of_i256_opt = IX256.narrow_of_x_opt
let narrow_of_i256_hlt = IX256.narrow_of_x_hlt

module IU256 = Convert.Make_nbI_wU(T)(U256)
let trunc_of_u256 = IU256.trunc_of_u
let narrow_of_u256_opt = IU256.narrow_of_u_opt
let widen_to_u256_opt = IU256.widen_to_u_opt
let narrow_of_u256_hlt = IU256.narrow_of_u_hlt
let widen_to_u256_hlt = IU256.widen_to_u_hlt

module IX128 = Convert.Make_nbI_wX(T)(I128)
let trunc_of_i128 = IX128.trunc_of_x
let extend_to_i128 = IX128.extend_to_x
let narrow_of_i128_opt = IX128.narrow_of_x_opt
let narrow_of_i128_hlt = IX128.narrow_of_x_hlt

module IU128 = Convert.Make_nbI_wU(T)(U128)
let trunc_of_u128 = IU128.trunc_of_u
let narrow_of_u128_opt = IU128.narrow_of_u_opt
let widen_to_u128_opt = IU128.widen_to_u_opt
let narrow_of_u128_hlt = IU128.narrow_of_u_hlt
let widen_to_u128_hlt = IU128.widen_to_u_hlt
