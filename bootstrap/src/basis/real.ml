open Rudiments0

module T = struct
  type t = real

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u64 1L ~f:(fun _ -> Int64.bits_of_float t)
    |> Hash.State.Gen.fini

  let cmp t0 t1 =
    let rel = Sint.extend_of_int (compare t0 t1) in
    if Sint.(rel < 0L) then
      Cmp.Lt
    else if Sint.(rel = 0L) then
      Cmp.Eq
    else
      Cmp.Gt

  let pp ppf t =
    Format.fprintf ppf "%h" t

  let of_string s =
    float_of_string s

  let to_string t =
    string_of_float t

  let zero = 0.
end
include T
include Identifiable.Make(T)
include Cmpable.MakeZero(T)

let of_sint x =
  float_of_int (int_of_sint x)

let to_sint t =
  sint_of_int (int_of_float t)

module Dir = struct
  type t =
    | Down
    | Up
    | Nearest
    | Zero

  let pp ppf t =
    Format.fprintf ppf (match t with
      | Down -> "Down"
      | Up -> "Up"
      | Nearest -> "Nearest"
      | Zero -> "Zero"
    )
end

module Class = struct
  type t =
    | Infinite
    | Nan
    | Normal
    | Subnormal
    | Zero

  let pp ppf t =
    Format.fprintf ppf (match t with
      | Infinite -> "Infinite"
      | Nan -> "Nan"
      | Normal -> "Normal"
      | Subnormal -> "Subnormal"
      | Zero -> "Zero"
    )
end

module Parts = struct
  type outer = t
  type t = {
    fractional: outer;
    integral: outer;
  }

  let fractional t =
    t.fractional

  let integral t =
    t.integral

  let pp ppf t =
    Format.fprintf ppf "@[<h>{fractional:@ %h,@ integral:@ %h}@]"
      t.fractional t.integral
end

let create ~neg ~exponent ~mantissa =
  assert Sint.(exponent >= (-1023L));
  assert Sint.(exponent <= 1024L);
  assert Uns.(mantissa <= 0xf_ffff_ffff_ffffL);
  let sign = match neg with
    | false -> Int64.zero
    | true -> Int64.one
  in
  let biased_exponent =
    Int64.of_int (int_of_sint Sint.(exponent + 1023L)) in
  let bits =
    Int64.logor (Int64.shift_left sign 63)
      (Int64.logor (Int64.shift_left biased_exponent 52) mantissa)
  in
  Int64.float_of_bits bits

let is_neg t =
  let bits = Int64.bits_of_float t in
  let sign = Int64.shift_right_logical bits 63 in
  Int64.equal sign Int64.one

let exponent t =
  let bits = Int64.bits_of_float t in
  let biased_exponent = sint_of_int (Int64.to_int (
    Int64.logand (Int64.shift_right_logical bits 52) 0x7ffL)) in
  Sint.(biased_exponent - 1023L)

let mantissa t =
  let bits = Int64.bits_of_float t in
  Int64.logand bits (Int64.of_int 0xf_ffff_ffff_ffff)

let m2x t =
  let f, x = frexp t in
  f, (sint_of_int x)

let f2x ~p t =
  ldexp t (int_of_sint p)

let modf t =
  let fractional, integral = modf t in
  {Parts. fractional; integral}

let min_value = neg_infinity
let max_value = infinity

let one = 1.
let neg_one = -1.
let nan = nan
let inf = infinity
let neg_inf = neg_infinity
let pi = 0x3.243f6a8885a3

let ( + ) t0 t1 =
  t0 +. t1

let ( - ) t0 t1 =
  t0 -. t1

let ( * ) t0 t1 =
  t0 *. t1

let ( / ) t0 t1 =
  t0 /. t1

let ( % ) t0 t1 =
  mod_float t0 t1

let ( ** ) t0 t1 =
  Stdlib.( ** ) t0 t1

let ( ~- ) t =
  -1. * t

let ( ~+) t =
  t

let neg t =
  -1. * t

let abs t =
  match is_neg t with
  | false -> t
  | true -> -1. * t

let copysign ~sign t =
  copysign t sign

let classify t =
  match classify_float t with
  | FP_infinite -> Class.Infinite
  | FP_nan -> Class.Nan
  | FP_normal -> Class.Normal
  | FP_subnormal -> Class.Subnormal
  | FP_zero -> Class.Zero

let is_nan t =
  match classify t with
  | Nan -> true
  | _ -> false

let is_inf t =
  match classify t with
  | Infinite -> true
  | _ -> false

let is_fin t =
  match classify t with
  | Infinite
  | Nan -> false
  | Normal
  | Subnormal
  | Zero -> true

let round ?(dir=Dir.Nearest) t =
  match dir with
  | Down -> floor t
  | Up -> ceil t
  | Nearest -> begin
      let lb = -0x1p52 in
      let ub = 0x1p52 in
      if t >= lb && t <= ub then
        if is_neg t then ceil (t - 0.5)
        else floor (t + 0.5)
      else
        t
    end
  | Zero -> Parts.integral (modf t)

let min t0 t1 =
  match cmp t0 t1 with
  | Lt
  | Eq -> t0
  | Gt -> t1

let max t0 t1 =
  match cmp t0 t1 with
  | Lt -> t1
  | Eq
  | Gt -> t0

let ex t =
  exp t

let exm1 t =
  expm1 t

let ln t =
  log t

let lg t =
  (ln t) / (ln 2.)

let ln1p t =
  log1p t

let log t =
  log10 t

let pow ~p t =
  t ** p

let int_pow ~p t =
  (* Decompose the exponent to limit algorithmic complexity. *)
  let neg, n = if Sint.(is_negative p) then
      true, Sint.(-p)
    else
      false, p
  in
  let rec fn r p n = begin
    match n with
    | 0L -> r
    | _ -> begin
        let r' = match Uns.bit_and n 1L with
          | 0L -> r
          | 1L -> r * p
          | _ -> not_reached ()
        in
        let p' = p * p in
        let n' = Uns.bit_usr ~shift:1L n in
        fn r' p' n'
      end
  end in
  let r = fn 1. t (Uns.bits_of_sint n) in
  match neg with
  | false -> r
  | true -> 1. / r

let lngamma_impl t =
  let f, t' = match t < 7. with
    | true -> begin
        let rec fn f z = begin
          match z < 7. with
          | false -> -ln f, z
          | true -> fn (f * z) (z + 1.)
        end in
        fn t (t + 1.)
      end
    | false -> 0., t
  in
  let z = 1. / (t' * t') in
  let g = f + (t' - 0.5) * (ln t') - t' + 0.918938533204673 + (((-0.000595238095238 * z +
      0.000793650793651) * z - 0.002777777777778) * z + 0.083333333333333) / t'
  in
  match t <= 1. || t >= 2. with
  | true -> abs g
  | false -> -abs g

let lngamma t =
  match classify t with
  | Nan -> nan
  | Infinite
  | Zero -> inf
  | Normal
  | Subnormal -> begin
      if t < 0. then inf
      else lngamma_impl t
    end

let gamma t =
  match classify t with
  | Nan -> nan
  | Infinite -> begin
      if is_neg t then nan
      else inf
    end
  | Zero -> copysign ~sign:t inf
  | Normal
  | Subnormal -> begin
      if t < 0. then nan
      else ex (lngamma_impl t)
    end

let sqrt t =
  sqrt t

let cbrt t =
  match classify t with
  | Infinite
  | Nan
  | Zero -> t
  | Normal
  | Subnormal -> begin
      if is_neg t then -((-t) ** (1./3.))
      else t ** (1./3.)
    end

let hypot t0 t1 =
  match classify t0, classify t1 with
  | Infinite, Nan -> t0
  | Nan, Infinite -> t1
  | _, _ -> sqrt (t0*t0 + t1*t1)

let sin t =
  sin t

let cos t =
  cos t

let tan t =
  tan t

let asin t =
  asin t

let acos t =
  acos t

let atan t =
  atan t

let atan2 t0 t1 =
  atan2 t0 t1

let sinh t =
  sinh t

let cosh t =
  cosh t

let tanh t =
  tanh t

module O = struct
  module T = struct
    type nonrec t = t

    let cmp = cmp
  end
  include T
  include Cmpable.Make(T)

  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( % ) = ( % )
  let ( ** ) = ( ** )
  let ( ~- ) = ( ~- )
  let ( ~+ ) = ( ~+ )
  let neg = neg
  let abs = abs
end
