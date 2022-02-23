open Rudiments0

module T = struct
  type t = real

  module Dir = struct
    type t =
      | Down
      | Up
      | Nearest
      | Zero

    let pp t formatter =
      formatter |> Fmt.fmt (match t with
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

    let pp t formatter =
      formatter |> Fmt.fmt (match t with
        | Infinite -> "Infinite"
        | Nan -> "Nan"
        | Normal -> "Normal"
        | Subnormal -> "Subnormal"
        | Zero -> "Zero"
      )
  end

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

  let of_sint x =
    float_of_int (int_of_sint x)

  let to_sint t =
    sint_of_int (int_of_float t)

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

  let min_value = neg_infinity
  let max_value = infinity

  let zero = 0.
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
          let n' = Uns.bit_sr ~shift:1L n in
          fn r' p' n'
        end
    end in
    let r = fn 1. t (Uns.bits_of_sint n) in
    match neg with
    | false -> r
    | true -> 1. / r

  let lngamma_impl t =
    let f, t' = match Cmp.is_lt (cmp t 7.) with
      | true -> begin
          let rec fn f z = begin
            match Cmp.is_lt (cmp z 7.) with
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
    match Cmp.is_le (cmp t 1.) || Cmp.is_ge (cmp t 2.) with
    | true -> abs g
    | false -> -abs g

  let lngamma t =
    match classify t with
    | Nan -> nan
    | Infinite
    | Zero -> inf
    | Normal
    | Subnormal -> begin
        if Cmp.is_lt (cmp t 0.) then inf
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
        if Cmp.is_lt (cmp t 0.) then nan
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

  let of_string s =
    let open Radix in
    let result_of_accums radix ~e ~m = begin
      match radix with
      | Dec -> m *. (10. ** e)
      | Bin
      | Oct
      | Hex -> m *. (2. ** e)
    end in
    let r3 radix ~e ~m cursor = begin
      let cp, _cursor' = String.C.Cursor.next cursor in
      match cp with
      | cp when Codepoint.(cp = of_char '2') -> result_of_accums radix ~e ~m
      | _ -> halt "Malformed real constant"
    end in
    let r6 radix ~e ~m cursor = begin
      let cp, _cursor' = String.C.Cursor.next cursor in
      match cp with
      | cp when Codepoint.(cp = of_char '4') -> result_of_accums radix ~e ~m
      | _ -> halt "Malformed real constant"
    end in
    let r radix ~e ~m cursor tl = begin
      match String.C.Cursor.(=) cursor tl with
      | true -> result_of_accums radix ~e ~m
      | false -> begin
          let cp, cursor' = String.C.Cursor.next cursor in
          match cp with
          | cp when Codepoint.(cp = of_char '3') -> r3 radix ~e ~m cursor'
          | cp when Codepoint.(cp = of_char '6') -> r6 radix ~e ~m cursor'
          | _ -> halt "Malformed real constant"
        end
    end in
    let rec exp radix eaccum esign ~m cursor tl = begin
      match String.C.Cursor.(=) cursor tl with
      | true -> result_of_accums radix ~e:(Sign.to_real esign *. eaccum) ~m
      | false -> begin
          let cp, cursor' = String.C.Cursor.next cursor in
          match radix, cp with
          | _, cp when Codepoint.(cp = of_char '0') ->
            exp radix (eaccum *. 10. +. 0.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '1') ->
            exp radix (eaccum *. 10. +. 1.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '2') ->
            exp radix (eaccum *. 10. +. 2.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '3') ->
            exp radix (eaccum *. 10. +. 3.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '4') ->
            exp radix (eaccum *. 10. +. 4.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '5') ->
            exp radix (eaccum *. 10. +. 5.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '6') ->
            exp radix (eaccum *. 10. +. 6.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '7') ->
            exp radix (eaccum *. 10. +. 7.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '8') ->
            exp radix (eaccum *. 10. +. 8.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '9') ->
            exp radix (eaccum *. 10. +. 9.) esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char '_') -> exp radix eaccum esign ~m cursor' tl
          | _, cp when Codepoint.(cp = of_char 'r') ->
            r radix ~e:(Sign.to_real esign *. eaccum) ~m cursor' tl
          | _ -> halt "Malformed real constant"
        end
    end in
    let rec ep_sign radix esign ~m cursor tl = begin
      let cp, cursor' = String.C.Cursor.next cursor in
      match cp with
      | cp when Codepoint.(cp = of_char '0') -> exp radix 0. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '1') -> exp radix 1. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '2') -> exp radix 2. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '3') -> exp radix 3. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '4') -> exp radix 4. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '5') -> exp radix 5. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '6') -> exp radix 6. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '7') -> exp radix 7. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '8') -> exp radix 8. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '9') -> exp radix 9. esign ~m cursor' tl
      | cp when Codepoint.(cp = of_char '_') -> ep_sign radix esign ~m cursor' tl
      | _ -> halt "Malformed real constant"
    end in
    let rec ep radix ~m cursor tl = begin
      let cp, cursor' = String.C.Cursor.next cursor in
      match cp with
      | cp when Codepoint.(cp = of_char '-') -> ep_sign radix Sign.Neg ~m cursor' tl
      | cp when Codepoint.(cp = of_char '+') -> ep_sign radix Sign.Pos ~m cursor' tl
      | cp when Codepoint.(cp = of_char '_') -> ep radix ~m cursor' tl
      | _ -> ep_sign radix Sign.Pos ~m cursor tl
    end in
    let rec frac ~ds ~b radix maccum msign cursor tl = begin
      (* (ds * digit) scales digit to its fractional value. *)
      let ds' = ds /. b in
      match String.C.Cursor.(=) cursor tl with
      | true -> maccum
      | false -> begin
          let cp, cursor' = String.C.Cursor.next cursor in
          match radix, cp with
          | _, cp when Codepoint.(cp = of_char '0') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 0.) msign cursor' tl
          | _, cp when Codepoint.(cp = of_char '1') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 1.) msign cursor' tl
          | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '2') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 2.) msign cursor' tl
          | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '3') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 3.) msign cursor' tl
          | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '4') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 4.) msign cursor' tl
          | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '5') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 5.) msign cursor' tl
          | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '6') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 6.) msign cursor' tl
          | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '7') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 7.) msign cursor' tl
          | (Dec|Hex), cp when Codepoint.(cp = of_char '8') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 8.) msign cursor' tl
          | (Dec|Hex), cp when Codepoint.(cp = of_char '9') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 9.) msign cursor' tl
          | Hex, cp when Codepoint.(cp = of_char 'a') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 0xa.) msign cursor' tl
          | Hex, cp when Codepoint.(cp = of_char 'b') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 0xb.) msign cursor' tl
          | Hex, cp when Codepoint.(cp = of_char 'c') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 0xc.) msign cursor' tl
          | Hex, cp when Codepoint.(cp = of_char 'd') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 0xd.) msign cursor' tl
          | Hex, cp when Codepoint.(cp = of_char 'e') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 0xe.) msign cursor' tl
          | Hex, cp when Codepoint.(cp = of_char 'f') ->
            frac ~ds:ds' ~b radix (maccum +. ds *. 0xf.) msign cursor' tl
          | _, cp when Codepoint.(cp = of_char '_') -> frac ~ds ~b radix maccum msign cursor' tl
          | Dec, cp when Codepoint.(cp = of_char 'e') ->
            ep radix ~m:(Sign.to_real msign *. maccum) cursor' tl
          | (Bin|Oct|Hex), cp when Codepoint.(cp = of_char 'p') ->
            ep radix ~m:(Sign.to_real msign *. maccum) cursor' tl
          | _, cp when Codepoint.(cp = of_char 'r') ->
            r radix ~e:0. ~m:(Sign.to_real msign *. maccum) cursor' tl
          | _ -> halt "Malformed real constant"
        end
    end in
    let rec whole ~b radix maccum msign cursor tl = begin
      let cp, cursor' = String.C.Cursor.next cursor in
      match radix, cp with
      | _, cp when Codepoint.(cp = of_char '0') ->
        whole ~b radix (maccum *. b +. 0.) msign cursor' tl
      | _, cp when Codepoint.(cp = of_char '1') ->
        whole ~b radix (maccum *. b +. 1.) msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '2') ->
        whole ~b radix (maccum *. b +. 2.) msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '3') ->
        whole ~b radix (maccum *. b +. 3.) msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '4') ->
        whole ~b radix (maccum *. b +. 4.) msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '5') ->
        whole ~b radix (maccum *. b +. 5.) msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '6') ->
        whole ~b radix (maccum *. b +. 6.) msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '7') ->
        whole ~b radix (maccum *. b +. 7.) msign cursor' tl
      | (Dec|Hex), cp when Codepoint.(cp = of_char '8') ->
        whole ~b radix (maccum *. b +. 8.) msign cursor' tl
      | (Dec|Hex), cp when Codepoint.(cp = of_char '9') ->
        whole ~b radix (maccum *. b +. 9.) msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'a') ->
        whole ~b radix (maccum *. b +. 0xa.) msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'b') ->
        whole ~b radix (maccum *. b +. 0xb.) msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'c') ->
        whole ~b radix (maccum *. b +. 0xc.) msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'd') ->
        whole ~b radix (maccum *. b +. 0xd.) msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'e') ->
        whole ~b radix (maccum *. b +. 0xe.) msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'f') ->
        whole ~b radix (maccum *. b +. 0xf.) msign cursor' tl
      | _, cp when Codepoint.(cp = of_char '_') -> whole ~b radix maccum msign cursor' tl
      | Dec, cp when Codepoint.(cp = of_char 'e') ->
        ep radix ~m:(Sign.to_real msign *. maccum) cursor' tl
      | (Bin|Oct|Hex), cp when Codepoint.(cp = of_char 'p') ->
        ep radix ~m:(Sign.to_real msign *. maccum) cursor' tl
      | _, cp when Codepoint.(cp = of_char '.') ->
        frac ~ds:(1. /. b) ~b radix maccum msign cursor' tl
      | _, cp when Codepoint.(cp = of_char 'r') ->
        r radix ~e:0. ~m:(Sign.to_real msign *. maccum) cursor' tl
      | _ -> halt "Malformed real constant"
    end in
    let rec radix ~b radix_ msign cursor tl = begin
      let cp, cursor' = String.C.Cursor.next cursor in
      match radix_, cp with
      | _, cp when Codepoint.(cp = of_char '0') -> whole ~b radix_ 0. msign cursor' tl
      | _, cp when Codepoint.(cp = of_char '1') -> whole ~b radix_ 1. msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '2') -> whole ~b radix_ 2. msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '3') -> whole ~b radix_ 3. msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '4') -> whole ~b radix_ 4. msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '5') -> whole ~b radix_ 5. msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '6') -> whole ~b radix_ 6. msign cursor' tl
      | (Oct|Dec|Hex), cp when Codepoint.(cp = of_char '7') -> whole ~b radix_ 7. msign cursor' tl
      | (Dec|Hex), cp when Codepoint.(cp = of_char '8') -> whole ~b radix_ 8. msign cursor' tl
      | (Dec|Hex), cp when Codepoint.(cp = of_char '9') -> whole ~b radix_ 9. msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'a') -> whole ~b radix_ 0xa. msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'b') -> whole ~b radix_ 0xb. msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'c') -> whole ~b radix_ 0xc. msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'd') -> whole ~b radix_ 0xd. msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'e') -> whole ~b radix_ 0xe. msign cursor' tl
      | Hex, cp when Codepoint.(cp = of_char 'f') -> whole ~b radix_ 0xf. msign cursor' tl
      | _, cp when Codepoint.(cp = of_char '_') -> radix ~b radix_ msign cursor' tl
      | _ -> halt "Malformed real constant"
    end in
    let zero msign cursor tl = begin
      let cp, cursor' = String.C.Cursor.next cursor in
      match cp with
      | cp when Codepoint.(cp = of_char 'b') -> radix ~b:2. Bin msign cursor' tl
      | cp when Codepoint.(cp = of_char 'o') -> radix ~b:8. Oct msign cursor' tl
      | cp when Codepoint.(cp = of_char 'x') -> radix ~b:0x10. Hex msign cursor' tl
      | cp when Codepoint.(cp = of_char '.') -> frac ~ds:(1. / 10.) ~b:10. Dec 0. msign cursor' tl
      | cp when Codepoint.(cp = of_char 'r') -> r Dec ~e:0. ~m:0. cursor' tl
      | _ -> whole ~b:10. Dec 0. msign cursor tl
    end in
    let msign sign cursor tl = begin
      let cp, cursor' = String.C.Cursor.next cursor in
      match cp with
      | cp when Codepoint.(cp = of_char '0') -> zero sign cursor' tl
      | _ -> radix ~b:10. Dec sign cursor tl
    end in
    match s with
    | "-inf" -> neg_inf
    | "inf" | "+inf" -> inf
    | "nan" -> nan
    | _ -> begin
        let cursor = String.C.Cursor.hd s in
        let tl = String.C.Cursor.tl s in
        let cp, cursor' = String.C.Cursor.next cursor in
        match cp with
        | cp when Codepoint.(cp = of_char '-') -> msign Sign.Neg cursor' tl
        | cp when Codepoint.(cp = of_char '+') -> msign Sign.Pos cursor' tl
        | _ -> msign Sign.Pos cursor tl
      end

  let to_string_inf ~sign t =
    match classify t with
    | Class.Infinite -> begin
        match sign, is_neg t with
        | Fmt.Implicit, false -> "inf"
        | Fmt.Explicit, false -> "+inf"
        | Fmt.Space, false -> " inf"
        | _, true -> "-inf"
      end
    | _ -> not_reached ()

  let to_string_nan t =
    match classify t with
    | Class.Nan -> "nan"
    | _ -> not_reached ()

  let fmt_sign neg ~sign is_exponent formatter =
    formatter
    |> Fmt.fmt (match neg, sign, is_exponent with
      | false, Fmt.Implicit, _ -> ""
      | false, Fmt.Explicit, _ -> "+"
      | false, Fmt.Space, false -> " "
      | false, Fmt.Space, true -> ""
      | true, _, _ -> "-"
    )

  let fmt_zpadding ~alt ~zpad ~width ~group prefix suffix formatter =
    (* ndigits incorporates the digit that is in the suffix. *)
    let rec fn ~alt ~zpad ~group ~zpad_rem:zpad_rem ~accum ~ndigits = begin
      match zpad, zpad_rem, Uns.(ndigits % group = 0L) with
      | false, _, _
      | _, 0L, _
      | true, 1L, true -> String.join accum
      | true, _, _ -> begin
          let zpad_rem', pad = match alt && Uns.(ndigits % group = 0L) with
            | true -> Uns.(zpad_rem - 2L), "0_"
            | false -> Uns.(zpad_rem - 1L), "0"
          in
          let accum' = pad :: accum in
          let ndigits' = succ ndigits in
          fn ~alt ~zpad ~group ~zpad_rem:zpad_rem' ~accum:accum' ~ndigits:ndigits'
        end
    end in
    let unpadded_length = Uns.(+) (String.C.length prefix) (String.C.length suffix) in
    match Uns.(unpadded_length < width) with
    | false -> formatter
    | true -> begin
        let zpad_rem = Uns.(width - unpadded_length) in
        Fmt.fmt (fn ~alt ~zpad ~group ~zpad_rem ~accum:[] ~ndigits:1L) formatter
      end

  (* Accurate to ~15 decimal digits. *)
  let to_string_e ~sign ~alt ~zpad ~width ~pmode ~precision ~notation t =
    (* Digit 0 is to the left of the radix point; fractional digits start at index 1. *)
    let digits ~precision ~notation abs_t = begin
      let compute_digit ~i ~e abs_t = begin
        (* The multiplier may not be normal, and therefore the result may be invalid. In such cases
         * try again with the multiplier split in two. This provides sufficient accuracy to compute
         * digits even for subnormals. *)
        assert (not (is_neg abs_t));
        let p_i = Sint.(+) (Sint.neg e) (Uns.bits_to_sint i) in
        let p = of_sint p_i in
        let pow_p_10 = pow ~p 10. in
        let digit = match classify pow_p_10 with
          | Zero
          | Normal
          | Subnormal -> (abs_t * pow_p_10) % 10.
          | Infinite -> begin
              let p2 = p * 0.5 in
              let pow_p2_10 = pow ~p:p2 10. in
              (abs_t * pow_p2_10 * pow_p2_10) % 10.
            end
          | Nan -> not_reached ()
        in
        Uns.of_real (match classify digit with
          | Zero
          | Normal
          | Subnormal -> floor digit
          | Infinite
          | Nan -> 0.
        )
      end in
      let rec round_dl ~e ~carry dl = begin
        match carry, dl with
        | false, _ -> dl, e
        | true, [] -> [1L], Sint.succ e
        | true, 9L :: dl' -> begin
            let dl'', e' = round_dl ~e ~carry:true dl' in
            (0L :: dl''), e'
          end
        | true, digit :: dl' -> begin
            let digit' = Uns.succ digit in
            assert Uns.(digit < 10L);
            (digit' :: dl'), e
          end
      end in
      let e = Sint.of_real (floor (log abs_t)) in
      let max_digit = match notation with
        | Fmt.Normalized -> succ precision
        | Fmt.RadixPoint -> Uns.bits_of_sint Sint.(max 0L (e + (succ (Uns.bits_to_sint precision))))
        | Fmt.Compact -> not_reached ()
      in
      (* Create a list with an extra digit (half_ulp), which will be rounded. *)
      let dl_unrounded = RangeF.Uns.fold RangeF.Uns.(0L =:= max_digit) ~init:[] ~f:(
        match classify abs_t with
        | Zero -> (fun digits _ -> 0L :: digits)
        | Normal
        | Subnormal -> (fun digits i -> (compute_digit ~i ~e abs_t) :: digits)
        | Infinite
        | Nan -> not_reached ()
      ) in
      let dl_rounded, e' = match dl_unrounded with
        | half_ulp :: dl_unrounded' -> round_dl ~e ~carry:Uns.(half_ulp >= 5L) dl_unrounded'
        | [] -> not_reached ()
      in
      (Array.of_list_rev dl_rounded), e'
    end in
    let digit = Array.get in
    let codepoint_of_digit u = begin
      Codepoint.narrow_of_uns_hlt (U8.extend_to_uns (String.B.get u "0123456789"))
    end in
    let fmt_mantissa ~alt ~pmode ~precision ~class_ ~digits formatter = begin
      formatter
      |> (match class_ with
        | Class.Zero
        | Class.Normal
        | Class.Subnormal -> (fun formatter ->
          let rec fn ~alt ~digits ~precision ~i formatter = begin
            match Uns.(i = precision) with
            | true -> formatter
            | false -> begin
                let sep = alt && Uns.(i % 3L = 0L) && Uns.(i > 0L) in
                let i' = succ i in
                formatter
                |> Fmt.fmt (match sep with false -> "" | true -> "_")
                |> Codepoint.fmt (codepoint_of_digit (digit i' digits))
                |> fn ~alt ~digits ~precision ~i:i'
              end
          end in
          (* If using limited precision, reduce precision to omit any trailing zeros. *)
          let precision = match pmode with
            | Fmt.Fixed -> precision
            | Fmt.Limited -> begin
                let rec compress ~precision ~digits = begin
                  match precision with
                  | 0L -> precision
                  | _ -> begin
                      match digit precision digits with
                      | 0L -> compress ~precision:(Uns.pred precision) ~digits
                      | _ -> precision
                    end
                end in
                compress ~precision ~digits
              end
          in
          formatter
          |> Codepoint.fmt (codepoint_of_digit (digit 0L digits))
          |> Fmt.fmt (match precision with 0L -> "" | _ -> ".")
          |> fn ~alt ~digits ~precision ~i:0L
        )
        | Class.Infinite
        | Class.Nan -> not_reached ()
      )
    end in
    let fmt_exponent ~alt ~sign class_ e formatter = begin
      formatter
      |> (match class_ with
        | Class.Zero -> (fun formatter ->
          formatter
          |> fmt_sign false ~sign true
          |> Fmt.fmt "0"
        )
        | Class.Normal
        | Class.Subnormal -> (fun formatter ->
          formatter
          |> fmt_sign Sint.(e < 0L) ~sign true
          |> Uns.fmt ~alt (Uns.bits_of_sint (Sint.abs e))
        )
        | Class.Infinite
        | Class.Nan -> not_reached ()
      )
    end in
    let fmt_whole ~alt ~digits ~e formatter = begin
      let rec fn ~digits ~e i formatter = begin
        match Uns.(i <= bits_of_sint e) with
        | false -> formatter
        | true -> begin
            formatter
            |> Fmt.fmt (match alt && Uns.((((bits_of_sint e) - (pred i)) % 3L) = 0L) with
              | true -> "_"
              | false -> ""
            )
            |> Codepoint.fmt (codepoint_of_digit (digit i digits))
            |> fn ~digits ~e (Uns.succ i)
          end
      end in
      formatter
      |> (match Sint.(e < 0L) with
        | true -> Fmt.fmt "0"
        | false -> (fun formatter ->
          formatter
          |> Codepoint.fmt (codepoint_of_digit (digit 0L digits))
          |> fn ~digits ~e 1L
        )
      )
    end in
    let fmt_part ~alt ~precision ~digits ~e formatter = begin
      let rec fn ~precision ~digits ~e i formatter = begin
        match Uns.(i <= precision) with
        | false -> formatter
        | true -> begin
            let pred_i = pred i in
            let digit_i = Sint.(Uns.bits_to_sint i + e) in
            formatter
            |> Fmt.fmt (match alt && Uns.(pred_i > 0L) && Uns.(pred_i % 3L = 0L) with
              | true -> "_"
              | false -> ""
            )
            |> (match Sint.(digit_i < 0L) with
              | true -> Fmt.fmt "0"
              | false ->
                Codepoint.fmt (codepoint_of_digit (digit Uns.(bits_of_sint digit_i) digits))
            )
            |> fn ~precision ~digits ~e (Uns.succ i)
          end
      end in
      formatter
      |> fn ~precision ~digits ~e 1L
    end in
    let fmt_denorm ~alt ~pmode ~precision ~digits ~e formatter = begin
      (* If using limited precision, reduce precision to omit any trailing zeros. *)
      let precision = match pmode with
        | Fmt.Fixed -> precision
        | Fmt.Limited -> begin
            let rec compress ~precision ~digits = begin
              match precision with
              | 0L -> precision
              | _ -> begin
                  let digit_i = Sint.(Uns.bits_to_sint precision + e) in
                  match Sint.(digit_i < 0L) || Uns.(=) (digit digit_i digits) 0L with
                  | true -> compress ~precision:(Uns.pred precision) ~digits
                  | false -> precision
                end
            end in
            compress ~precision ~digits
          end
      in
      formatter
      |> fmt_whole ~alt ~digits ~e
      |> Fmt.fmt "."
      |> fmt_part ~alt ~precision ~digits ~e
    end in
    match classify t with
    | Class.Zero
    | Class.Normal
    | Class.Subnormal as class_ -> begin
        let digits, e = digits ~precision ~notation (abs t) in
        let prefix =
          String.Fmt.empty
          |> fmt_sign (is_neg t) ~sign false
          |> Fmt.to_string
        in
        let suffix =
          String.Fmt.empty
          |> (match notation with
            | Fmt.Normalized -> (fun formatter ->
              formatter
              |> fmt_mantissa ~alt ~pmode ~precision ~class_ ~digits
              |> Fmt.fmt "e"
              |> fmt_exponent ~alt ~sign class_ e
            )
            | Fmt.RadixPoint -> fmt_denorm ~alt ~pmode ~precision ~digits ~e
            | Fmt.Compact -> not_reached ()
          )
          |> Fmt.to_string
        in
        String.Fmt.empty
        |> Fmt.fmt prefix
        |> fmt_zpadding ~alt ~zpad ~width ~group:3L prefix suffix
        |> Fmt.fmt suffix
        |> Fmt.to_string
      end
    | Class.Infinite -> to_string_inf ~sign t
    | Class.Nan -> to_string_nan t

  (* Bit-accurate. *)
  let to_string_p ~sign ~alt ~zpad ~width ~pmode ~precision ~notation ~radix t =
    let fmt_mantissa ~alt ~pmode ~precision ~class_ ~bits_per_digit ~group ~m ~sig_bits formatter
      = begin
        match class_ with
        | Class.Zero
        | Class.Normal
        | Class.Subnormal -> begin
            let rec fn ~alt ~plimited ~precision ~bits_per_digit ~group ~frac ~sig_bits ~ndigits
                formatter = begin
              let sep = alt && Uns.(ndigits % group = 0L) && Uns.(ndigits > 0L) in
              let ndigits' = succ ndigits in
              match Uns.(ndigits = precision) || (plimited && Uns.(frac = 0L)), Uns.(sig_bits = 0L)
              with
              | true, _ -> formatter
              | false, true -> begin
                  formatter
                  |> Fmt.fmt (match sep with false -> "" | true -> "_")
                  |> Fmt.fmt "0"
                  |> fn ~alt ~plimited ~precision ~bits_per_digit ~group ~frac ~sig_bits
                    ~ndigits:ndigits'
                end
              | false, false -> begin
                  let sig_bits' = Uns.(sig_bits - bits_per_digit) in
                  let digit = Uns.bit_sr ~shift:sig_bits' frac in
                  let mask = pred (bit_sl ~shift:sig_bits' 1L) in
                  let frac' = bit_and frac mask in
                  formatter
                  |> Fmt.fmt (match sep with false -> "" | true -> "_")
                  |> Codepoint.fmt (Codepoint.narrow_of_uns_hlt (U8.extend_to_uns
                      (String.B.get digit "0123456789abcdef")))
                  |> fn ~alt ~plimited ~precision ~bits_per_digit ~group ~frac:frac'
                    ~sig_bits:sig_bits' ~ndigits:ndigits'
                end
            end in
            let plimited = match pmode with
              | Fmt.Limited -> true
              | Fmt.Fixed -> false
            in
            formatter
            |> Fmt.fmt (match class_ with
              | Class.Zero -> "0"
              | Class.Normal
              | Class.Subnormal -> "1"
              | Class.Infinite
              | Class.Nan -> not_reached ()
            )
            |> Fmt.fmt (match plimited && Uns.(m = 0L) with false -> "." | true -> "")
            |> fn ~alt ~plimited ~precision ~bits_per_digit ~group ~frac:m ~sig_bits ~ndigits:0L
          end
        | Class.Infinite
        | Class.Nan -> not_reached ()
      end in
    let fmt_exponent ~sign ~alt class_ e formatter = begin
      formatter
      |> (match class_ with
        | Class.Zero -> (fun formatter ->
          formatter
          |> fmt_sign false ~sign true
          |> Fmt.fmt "0"
        )
        | Class.Normal
        | Class.Subnormal -> (fun formatter ->
          formatter
          |> fmt_sign Sint.(e < 0L) ~sign true
          |> Uns.fmt ~alt (Uns.bits_of_sint (Sint.abs e))
        )
        | Class.Infinite
        | Class.Nan -> not_reached ()
      )
    end in
    let fmt_norm ~sign ~alt ~pmode ~precision ~class_ ~bits_per_digit ~group ~m_norm ~e_norm
        ~sig_bits_norm formatter = begin
      (* Round if significant bits will be omitted. *)
      let m_rounded, e_rounded =
        match Uns.(precision * bits_per_digit < sig_bits_norm) with
        | false -> m_norm, e_norm
        | true -> begin
            (* Mask out trailing bits that won't be used so that in limited precision mode it is
             * trivial to recognize when no non-zero digits remain. *)
            let ulp = (bit_sl ~shift:Uns.((sig_bits_norm - (precision * bits_per_digit))) 1L) in
            let half_ulp = Uns.bit_sr ~shift:1L ulp in
            let rounded_mask = bit_not (pred ulp) in
            let m_rounded = Uns.(bit_and (m_norm + half_ulp) rounded_mask) in
            (* If rounding overflows, shift one bit out of the mantissa and correspondingly adjust
             * the exponent. *)
            let mask = pred (bit_sl ~shift:sig_bits_norm 1L) in
            match (bit_and m_rounded mask) = m_rounded with
            | true -> m_rounded, e_norm
            | false -> begin
                (* Rounding overflowed, so the resulting mantissa is zero because adding half_ulp
                 * can only overflow if all unrounded mantissa bits were ones. Increment the
                 * exponent. *)
                assert Uns.((bit_and m_rounded mask) = 0L);
                let m_overflowed = 0L in
                let e_overflowed = succ e_norm in
                m_overflowed, e_overflowed
              end
          end
      in
      (* Assure that sig_bits_shifted is evenly divisible by bits_per_digit, so that the least
       * significant digit doesn't require special handling. *)
      let m_shift = Uns.((bits_per_digit - (sig_bits_norm % bits_per_digit)) % bits_per_digit) in
      let m_shifted = Uns.bit_sl ~shift:m_shift m_rounded in
      let e_shifted = e_rounded in
      let sig_bits_shifted = Uns.(sig_bits_norm + m_shift) in
      let m, e, sig_bits = m_shifted, e_shifted, sig_bits_shifted in
      formatter
      |> fmt_mantissa ~alt ~pmode ~precision ~class_ ~bits_per_digit ~group ~m ~sig_bits
      |> Fmt.fmt "p"
      |> fmt_exponent ~sign ~alt class_ e
    end in
    (* Bit 0 is to the left of the normalized radix point; fractional bits start at index 1. Reading
     * implicit 0 bits at negative indices and positive indices past the explicitly stored mantissa
     * is intentionally supported to simplify boundary condition logic. *)
    let bit ~class_ ~m ~sig_bits (i:sint) = begin
      match class_, i with
      | Class.Zero, _ -> 0L
      | Class.Normal, _
      | Class.Subnormal, _
        when Sint.(i < 0L) -> 0L (* Preceding mantissa. *)
      | Class.Normal, 0L
      | Class.Subnormal, 0L -> 1L (* Implicit leading mantissa bit. *)
      | Class.Normal, _
      | Class.Subnormal, _
        when Uns.(sig_bits < (bits_of_sint i)) -> 0L (* Beyond explicit mantissa bits. *)
      | Class.Normal, _
      | Class.Subnormal, _ -> begin
          assert Uns.(sig_bits >= (bits_of_sint i));
          let shift = Uns.(sig_bits - (bits_of_sint i)) in
          Uns.bit_and (Uns.bit_sr ~shift m) 1L
        end
      | Class.Infinite, _
      | Class.Nan, _ -> not_reached ()
    end in
    let digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit = begin
      Range.fold (0L =:< bits_per_digit) ~init:0L ~f:(fun accum i ->
        Uns.(bit_or
            (bit_sl ~shift:1L accum)
            (bit ~class_ ~m ~sig_bits Sint.(msbit + (bits_to_sint i)))
        )
      )
    end in
    let cp_of_digit digit = begin
      Codepoint.narrow_of_uns_hlt (U8.extend_to_uns (String.B.get digit "0123456789abcdef"))
    end in
    let cp_digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit = begin
      cp_of_digit (digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit)
    end in
    let fmt_whole ~alt ~class_ ~bits_per_digit ~group ~m ~sig_bits ~nwhole ~msbit formatter =
      begin
        formatter
        |> (match nwhole with
          | 0L -> Fmt.fmt "0"
          | _ -> (fun formatter ->
            Range.fold (0L =:< nwhole) ~init:formatter ~f:(fun formatter i ->
              let msbit = Sint.(msbit + (Uns.bits_to_sint (i * bits_per_digit))) in
              formatter
              |> Fmt.fmt (
                match alt && Uns.(i > 0L) && Uns.((nwhole - i) % group = 0L) with
                | false -> ""
                | true -> "_"
              )
              |> Codepoint.fmt (cp_digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit)
            )
          )
        )
      end in
    let fmt_part ~alt ~pmode ~precision ~class_ ~bits_per_digit ~group ~m ~sig_bits ~msbit
        formatter =
      begin
        let dl = Range.fold (0L =:< precision) ~init:[] ~f:(fun digits i ->
          let msbit = Sint.(msbit + (Uns.bits_to_sint (i * bits_per_digit))) in
          (digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit) :: digits
        ) in
        let digits = Array.of_list_rev (match pmode with
          | Fmt.Fixed -> dl
          | Fmt.Limited -> begin
              (* Discard trailing 0 digits. *)
              let rec fn = function
                | [] -> []
                | 0L :: dl' -> fn dl'
                | dl -> dl
              in
              fn dl
            end
        ) in
        Array.foldi digits ~init:formatter ~f:(fun i formatter digit ->
          formatter
          |> Fmt.fmt (
            match alt && Uns.(i > 0L) && Uns.(i % group = 0L) with
            | false -> ""
            | true -> "_"
          )
          |> Codepoint.fmt (cp_of_digit digit)
        )
      end in
    (* Compute the number of whole digits and the index of the most significant bit to be
     * incorporated into the most significant non-zero digit. *)
    let nwhole_msbit ~class_ ~bits_per_digit ~e = begin
      match class_ with
      | Class.Zero -> 0L, 0L
      | Class.Normal
      | Class.Subnormal -> begin
          match Sint.(e < 0L) with
          | true -> 0L, (Sint.succ e)
          | false -> begin
              let nwhole = Uns.(((bits_of_sint e) + bits_per_digit) / bits_per_digit) in
              let msbit = Sint.(succ (e - (Uns.bits_to_sint (nwhole * bits_per_digit)))) in
              nwhole, msbit
            end
        end
      | Class.Infinite
      | Class.Nan -> not_reached ()
    end in
    let fmt_denorm ~alt ~pmode ~precision ~class_ ~bits_per_digit ~group ~m_norm ~e_norm ~sig_bits
        formatter = begin
      let nwhole_unrounded, msbit_unrounded = nwhole_msbit ~class_ ~bits_per_digit ~e:e_norm in
      (* Round if significant bits will be omitted. *)
      let m, nwhole, msbit =
        let lsbit = Sint.(pred msbit_unrounded + Uns.(bits_to_sint ((nwhole_unrounded + precision) *
            bits_per_digit))) in
        let explicit_bits_max = Uns.bits_to_sint sig_bits in
        match Sint.(lsbit >= explicit_bits_max) with
        | true -> m_norm, nwhole_unrounded, msbit_unrounded
        | false -> begin
            (* Mask out trailing bits that won't be used so that in limited precision mode it is
             * trivial to recognize when no non-zero digits remain. *)
            let ulp = (bit_sl ~shift:Uns.(bits_of_sint Sint.(explicit_bits_max - lsbit)) 1L) in
            let half_ulp = Uns.bit_sr ~shift:1L ulp in
            let rounded_mask = bit_not (pred ulp) in
            let m_rounded = Uns.(bit_and (m_norm + half_ulp) rounded_mask) in
            let mask = pred (bit_sl ~shift:sig_bits 1L) in
            match (bit_and m_rounded mask) = m_rounded with
            | true -> m_rounded, nwhole_unrounded, msbit_unrounded
            | false -> begin
                (* Rounding overflowed, so the resulting mantissa is zero because adding half_ulp
                 * can only overflow if all unrounded mantissa bits were ones. Increment the
                 * exponent and recompute whole/fractional digit layout. *)
                assert Uns.((bit_and m_rounded mask) = 0L);
                let m_overflowed = 0L in
                let e_overflowed = succ e_norm in
                let nwhole_overflowed, msbit_overflowed = nwhole_msbit ~class_ ~bits_per_digit
                    ~e:e_overflowed in
                m_overflowed, nwhole_overflowed, msbit_overflowed
              end
          end
      in
      let msbit_part = Sint.(msbit + Uns.(bits_to_sint (nwhole * bits_per_digit))) in
      formatter
      |> fmt_whole ~alt ~class_ ~bits_per_digit ~group ~m ~sig_bits ~nwhole ~msbit
      |> Fmt.fmt "."
      |> fmt_part ~alt ~pmode ~precision ~class_ ~bits_per_digit ~group ~m ~msbit:msbit_part
        ~sig_bits
    end in
    match classify t with
    | Class.Normal
    | Class.Subnormal
    | Class.Zero as class_ -> begin
        let open Radix in
        let bits_per_digit, group = match radix with
          | Bin -> 1L, 8L
          | Oct -> 3L, 3L
          | Dec -> not_reached ()
          | Hex -> 4L, 4L
        in
        (* Normalize subnormals. *)
        let sig_bits = 52L in
        let m_norm, e_norm, sig_bits_norm = match class_ with
          | Class.Normal -> mantissa t, exponent t, sig_bits
          | Class.Subnormal -> begin
              (* Shift/mask one significant bit out of the mantissa and correspondingly adjust the
               * exponent. This transformation allows all subsequent operations on
               * {Normal,Subnormal} inputs to be coded identically. *)
              let m_subnormal = mantissa t in
              let shift = Uns.(bit_clz m_subnormal - 11L) in
              let mask = pred (bit_sl ~shift:sig_bits 1L) in
              let m = bit_and (bit_sl ~shift m_subnormal) mask in
              let e = Sint.((-1022L) - shift) in
              m, e, sig_bits
            end
          | Class.Zero -> 0L, 0L, 0L
          | Class.Infinite
          | Class.Nan -> not_reached ()
        in
        let prefix =
          let open Radix in
          String.Fmt.empty
          |> fmt_sign (is_neg t) ~sign false
          |> Fmt.fmt (match radix with
            | Bin -> "0b"
            | Oct -> "0o"
            | Dec -> not_reached ()
            | Hex -> "0x"
          )
          |> Fmt.to_string
        in
        let suffix =
          String.Fmt.empty
          |> (match notation with
            | Fmt.Normalized -> fmt_norm ~sign ~alt ~pmode ~precision ~class_ ~bits_per_digit
                ~group ~m_norm ~e_norm ~sig_bits_norm
            | Fmt.RadixPoint -> fmt_denorm ~alt ~pmode ~precision ~class_ ~bits_per_digit ~group
                ~m_norm ~e_norm ~sig_bits:sig_bits_norm
            | Fmt.Compact -> not_reached ()
          )
          |> Fmt.to_string
        in
        String.Fmt.empty
        |> Fmt.fmt prefix
        |> fmt_zpadding ~alt ~zpad ~width ~group prefix suffix
        |> Fmt.fmt suffix
        |> Fmt.to_string
      end
    | Class.Infinite -> to_string_inf ~sign t
    | Class.Nan -> to_string_nan t

  let to_string_impl ?(sign=Fmt.sign_default) ?(alt=Fmt.alt_default) ?(zpad=Fmt.zpad_default)
    ?(width=Fmt.width_default) ?(pmode=Fmt.pmode_default) ?precision ~notation ~radix t =
    let open Radix in
    let precision = match precision, radix, notation with
      | Some precision, _, Fmt.(Normalized|RadixPoint) -> precision
      | None, Bin, Fmt.Normalized -> Fmt.precision_bin_m_default
      | None, Bin, Fmt.RadixPoint -> Fmt.precision_bin_a_default
      | None, Oct, Fmt.Normalized -> Fmt.precision_oct_m_default
      | None, Oct, Fmt.RadixPoint -> Fmt.precision_oct_a_default
      | None, Dec, Fmt.Normalized -> Fmt.precision_dec_m_default
      | None, Dec, Fmt.RadixPoint -> Fmt.precision_dec_a_default
      | None, Hex, Fmt.Normalized -> Fmt.precision_hex_m_default
      | None, Hex, Fmt.RadixPoint -> Fmt.precision_hex_a_default
      | _, _, Fmt.Compact -> not_reached ()
    in
    match radix with
    | Dec -> to_string_e ~sign ~alt ~zpad ~width ~pmode ~precision ~notation t
    | Bin
    | Oct
    | Hex -> to_string_p ~sign ~alt ~zpad ~width ~pmode ~precision ~notation ~radix t

  let to_string ?sign ?alt ?zpad ?width ?pmode ?precision ?notation ?(radix=Fmt.radix_default) t =
    let notation = match notation with
      | Some notation -> notation
      | None -> Fmt.notation_default
    in
    match notation with
    | Fmt.Normalized
    | Fmt.RadixPoint -> to_string_impl ?sign ?alt ?zpad ?width ?pmode ?precision ~notation ~radix t
    | Fmt.Compact -> begin
        let s_m = to_string_impl ?sign ?alt ?zpad ?width ?pmode ?precision ~notation:Fmt.Normalized
            ~radix t in
        let s_a = to_string_impl ?sign ?alt ?zpad ?width ?pmode ?precision ~notation:Fmt.RadixPoint
            ~radix t in
        match Cmp.is_lt (Uns.cmp (String.C.length s_m) (String.C.length s_a)) with
        | true -> s_m
        | false -> s_a
      end

  let fmt ?pad ?just ?sign ?alt ?zpad ?width ?pmode ?precision ?notation ?radix t formatter =
    Fmt.fmt ?pad ?just ?width (to_string ?sign ?alt ?zpad ?width ?pmode ?precision ?notation ?radix
        t) formatter

  let pp t formatter =
    fmt t formatter

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

    let pp t formatter =
      formatter
      |> Fmt.fmt "{fractional: "
      |> fmt ~radix:Radix.Hex t.fractional
      |> Fmt.fmt ", integral: "
      |> fmt ~radix:Radix.Hex t.integral
      |> Fmt.fmt "}"
  end

  let modf t =
    let fractional, integral = modf t in
    {Parts. fractional; integral}

  let round ?(dir=Dir.Nearest) t =
    match dir with
    | Down -> floor t
    | Up -> ceil t
    | Nearest -> begin
        let lb = -0x1p52 in
        let ub = 0x1p52 in
        if Cmp.is_ge (cmp t lb) && Cmp.is_le (cmp t ub) then
          if is_neg t then ceil (t - 0.5)
          else floor (t + 0.5)
        else
          t
      end
    | Zero -> Parts.integral (modf t)

end
include T
include Identifiable.Make(T)
include Cmpable.MakeZero(T)

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
