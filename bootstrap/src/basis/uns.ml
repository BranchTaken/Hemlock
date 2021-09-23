open RudimentsInt0
open RudimentsFunctions

module T = struct
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

  let pp ppf t =
    Format.fprintf ppf "%Lu" t
end
include T
include Cmpable.MakeZero(T)
include Identifiable.Make(T)

let pp_b ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Stdlib.(shift mod 8 = 0 && shift < 64) then Format.fprintf ppf "_";
        let shift' = Stdlib.(pred shift) in
        let bit = Int64.(logand (shift_right_logical x shift') 0x1L) in
        Format.fprintf ppf "%Ld" bit;
        fn x shift'
      end
  end in
  Format.fprintf ppf "0b";
  fn t 64

let pp_o ppf t =
  Format.fprintf ppf "0o%Lo" t

let pp_x ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Stdlib.(shift < 64) then Format.fprintf ppf "_";
        let shift' = Stdlib.(shift - 16) in
        Format.fprintf ppf "%04Lx" Int64.(logand (shift_right_logical x shift') 0xffffL);
        fn x shift'
      end
  end in
  Format.fprintf ppf "0x";
  fn t 64

let of_string s =
  match Stdlib.String.split_on_char 'x' s with
  | "0" :: _ -> Int64.of_string s (* Has 0x prefix. *)
  | _ -> begin
      (* Prefix with "0u" so that the string is interpreted as unsigned. *)
      Int64.of_string ("0u" ^ s)
    end

let to_string t =
  Format.asprintf "%a" pp t

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

let to_i64 t =
  t

let of_i64 i =
  i

let to_sint t =
  t

let of_sint i =
  i

let to_int t =
  Int64.to_int t

let to_int_opt t =
  match Stdlib.(t > (Int64.of_int Int.max_int)) with
  | true -> None
  | false -> Some (Int64.to_int t)

let to_int_hlt t =
  match to_int_opt t with
  | None -> halt "Lossy conversion"
  | Some x -> x

let of_int x =
  Int64.of_int x

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

module U = struct
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
include Intnb.MakeDerived(U)
