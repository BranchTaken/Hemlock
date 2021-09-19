open Rudiments0

module T = struct
  type t = u64

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u64 1 ~f:(fun _ -> t)
    |> Hash.State.Gen.fini

  let cmp t0 t1 =
    Sint.cmp (Uns.to_sint Int64.(unsigned_compare t0 t1)) (Sint.kv 0)

  let zero = Int64.zero

  let one = Int64.one

  let pp ppf t =
    Format.fprintf ppf "%Luu64" t
end
include T
include Cmpable.MakeZero(T)
include Identifiable.Make(T)

let pp_b ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Uns.(shift % 8 = 0 && shift < 64) then Format.fprintf ppf "_";
        let shift' = Uns.pred shift in
        let bit = Int64.(logand (shift_right_logical x shift') (of_int 0x1)) in
        Format.fprintf ppf "%Ld" bit;
        fn x shift'
      end
  end in
  Format.fprintf ppf "0b";
  fn t 64;
  Format.fprintf ppf "u64"

let pp_o ppf t =
  Format.fprintf ppf "0o%Lou64" t

let pp_x ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Uns.(shift < 64) then Format.fprintf ppf "_";
        let shift' = shift - 16 in
        Format.fprintf ppf "%04Lx" Int64.(logand (shift_right_logical x shift') (of_int 0xffff));
        fn x shift'
      end
  end in
  Format.fprintf ppf "0x";
  fn t 64;
  Format.fprintf ppf "u64"

let of_string s =
  match Caml.String.split_on_char 'x' s with
  | "0" :: _ -> Int64.of_string s (* Has 0x prefix. *)
  | _ -> begin
      (* Prefix with "0u" so that the string is interpreted as unsigned. *)
      Int64.of_string ("0u" ^ s)
    end

let to_string t =
  Format.asprintf "%a" pp t

let cf_ffff_ffff_ffff = Int64.of_string "0xf_ffff_ffff_ffff"
let c7ff = Int64.of_string "0x7ff"
let c10_0000_0000_0000 = Int64.of_string "0x10_0000_0000_0000"

let of_real r =
  match Real.classify r with
  | Normal -> begin
      match Real.(r >= 0.) with
      | false -> zero
      | true -> begin
          let bits = Int64.bits_of_float r in
          let biased_exponent = Int64.(to_int (logand (shift_right_logical bits 52) c7ff)) in
          match RudimentsInt.(biased_exponent >= 1023) with
          | false -> zero
          | true -> begin
              let exponent = biased_exponent - 1023 in
              let significand = Int64.(logor c10_0000_0000_0000 (logand bits cf_ffff_ffff_ffff)) in
              if RudimentsInt.(exponent < 52) then
                Int64.shift_right_logical significand (52 - exponent)
              else if RudimentsInt.(exponent < 116) then
                Int64.shift_left significand (exponent - 52)
              else
                zero
            end
        end
    end
  | Subnormal
  | Zero
  | Infinite -> zero
  | Nan -> halt "Not a number"

let c8000_0000_0000_0000 = of_string "0x8000_0000_0000_0000"
let c43e0_0000_0000_0000 = of_string "0x43e0_0000_0000_0000"

let to_real t =
  match RudimentsInt.(Int64.(compare (logand t c8000_0000_0000_0000) zero) = 0)
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

let to_uns t =
  Int64.to_int t

let to_uns_opt t =
  Int64.unsigned_to_int t

let to_uns_hlt t =
  match to_uns_opt t with
  | None -> halt "Lossy conversion"
  | Some x -> x

let of_uns u =
  let i = Uns.to_sint u in
  match Sint.(i >= (kv 0)) with
  | true -> Int64.of_int u
  | false -> begin
      let sint_sign_bit = Uns.of_sint Sint.min_value in
      Int64.(add (of_int u) (add (of_int sint_sign_bit) (of_int sint_sign_bit)))
    end

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
  Int64.shift_left t shift

let bit_usr ~shift t =
  Int64.shift_right_logical t shift

let bit_ssr ~shift t =
  Int64.shift_right t shift

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
        let n' = bit_usr ~shift:1 n in
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

  let bit_length = 64

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
include Intnb.MakeDerived(U)
