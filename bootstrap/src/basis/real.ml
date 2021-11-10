open Rudiments0

module T = struct
  type t = real

  module Dir = struct
    type t =
      | Down
      | Up
      | Nearest
      | Zero

    let xpp xppf t =
      Format.fprintf xppf (match t with
        | Down -> "Down"
        | Up -> "Up"
        | Nearest -> "Nearest"
        | Zero -> "Zero"
      )

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

    let xpp xppf t =
      Format.fprintf xppf (match t with
        | Infinite -> "Infinite"
        | Nan -> "Nan"
        | Normal -> "Normal"
        | Subnormal -> "Subnormal"
        | Zero -> "Zero"
      )

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
          let n' = Uns.bit_usr ~shift:1L n in
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

  let xpp xppf t =
    Format.fprintf xppf "%h" t

  let of_string s =
    float_of_string s

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
      | true, 1L, true -> String.concat accum
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
  let to_string_e ~sign ~alt ~zpad ~width ~precision ~notation t =
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
    let fmt_frac ~alt ~precision ~digits formatter = begin
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
      formatter
      |> Codepoint.fmt (codepoint_of_digit (digit 0L digits))
      |> Fmt.fmt "."
      |> fn ~alt ~digits ~precision ~i:0L
    end in
    let fmt_mantissa ~alt ~precision ~class_ ~digits formatter = begin
      formatter
      |> (match class_ with
        | Class.Zero -> Fmt.fmt "0"
        | Class.Normal
        | Class.Subnormal -> fmt_frac ~alt ~precision ~digits
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
    let fmt_denorm ~alt ~precision ~digits ~e formatter = begin
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
              |> fmt_mantissa ~alt ~precision ~class_ ~digits
              |> Fmt.fmt "e"
              |> fmt_exponent ~alt ~sign class_ e
            )
            | Fmt.RadixPoint -> fmt_denorm ~alt ~precision ~digits ~e
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
  let to_string_p ~sign ~alt ~zpad ~width ~precision ~notation ~base t =
    let fmt_frac ~alt ~precision ~bits_per_digit ~group ~frac ~sig_bits formatter = begin
      let rec fn ~alt ~precision ~bits_per_digit ~group ~frac ~sig_bits ~ndigits formatter = begin
        let sep = alt && Uns.(ndigits % group = 0L) && Uns.(ndigits > 0L) in
        let ndigits' = succ ndigits in
        match Uns.(ndigits = precision), Uns.(sig_bits = 0L) with
        | true, _ -> formatter
        | false, true -> begin
            formatter
            |> Fmt.fmt (match sep with false -> "" | true -> "_")
            |> Fmt.fmt "0"
            |> fn ~alt ~precision ~bits_per_digit ~group ~frac ~sig_bits ~ndigits:ndigits'
          end
        | false, false -> begin
            let sig_bits' = Uns.(sig_bits - bits_per_digit) in
            let digit = Uns.bit_usr ~shift:sig_bits' frac in
            let mask = pred (bit_sl ~shift:sig_bits' 1L) in
            let frac' = bit_and frac mask in
            formatter
            |> Fmt.fmt (match sep with false -> "" | true -> "_")
            |> Codepoint.fmt (Codepoint.narrow_of_uns_hlt (U8.extend_to_uns
                (String.B.get digit "0123456789abcdef")))
            |> fn ~alt ~precision ~bits_per_digit ~group ~frac:frac' ~sig_bits:sig_bits'
              ~ndigits:ndigits'
          end
      end in
      formatter
      |> Fmt.fmt "1."
      |> fn ~alt ~precision ~bits_per_digit ~group ~frac ~sig_bits ~ndigits:0L
    end in
    let fmt_mantissa ~alt ~precision ~class_ ~bits_per_digit ~group ~m ~sig_bits formatter = begin
      formatter
      |> (match class_ with
        | Class.Zero -> Fmt.fmt "0"
        | Class.Normal
        | Class.Subnormal -> fmt_frac ~alt ~precision ~bits_per_digit ~group ~frac:m ~sig_bits
        | Class.Infinite
        | Class.Nan -> not_reached ()
      )
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
    let fmt_norm ~sign ~alt ~precision ~class_ ~bits_per_digit ~group ~m_norm ~e_norm ~sig_bits_norm
        formatter = begin
      (* Round if significant bits will be omitted. *)
      let m_rounded, e_rounded, sig_bits_rounded =
        match Uns.(precision * bits_per_digit < sig_bits_norm) with
        | false -> m_norm, e_norm, sig_bits_norm
        | true -> begin
            let half_ulp = (bit_sl ~shift:Uns.(pred ((sig_bits_norm - (precision *
                bits_per_digit)))) 1L) in
            let m_rounded = Uns.(m_norm + half_ulp) in
            (* If rounding overflows, shift one bit out of the mantissa and correspondingly adjust
             * the exponent. *)
            let mask = pred (bit_sl ~shift:sig_bits_norm 1L) in
            match (bit_and m_rounded mask) = m_rounded with
            | true -> m_rounded, e_norm, sig_bits_norm
            | false -> m_rounded, (succ e_norm), (succ sig_bits_norm)
          end
      in
      (* Assure that sig_bits_shifted is evenly divisible by bits_per_digit, so that the least
       * significant digit doesn't require special handling. *)
      let m_shift = Uns.((bits_per_digit - (sig_bits_rounded % bits_per_digit)) % bits_per_digit) in
      let m_shifted = Uns.bit_sl ~shift:m_shift m_rounded in
      let e_shifted = e_rounded in
      let sig_bits_shifted = Uns.(sig_bits_rounded + m_shift) in
      let m, e, sig_bits = m_shifted, e_shifted, sig_bits_shifted in
      formatter
      |> fmt_mantissa ~alt ~precision ~class_ ~bits_per_digit ~group ~m ~sig_bits
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
          Uns.bit_and (Uns.bit_usr ~shift m) 1L
        end
      | Class.Infinite, _
      | Class.Nan, _ -> not_reached ()
    end in
    let digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit = begin
      let bits = Range.fold (0L =:< bits_per_digit) ~init:0L ~f:(fun accum i ->
        Uns.(bit_or
            (bit_sl ~shift:1L accum)
            (bit ~class_ ~m ~sig_bits Sint.(msbit + (bits_to_sint i)))
        )
      ) in
      Codepoint.narrow_of_uns_hlt (U8.extend_to_uns (String.B.get bits "0123456789abcdef"))
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
              |> Codepoint.fmt (digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit)
            )
          )
        )
      end in
    let fmt_part ~alt ~precision ~class_ ~bits_per_digit ~group ~m ~sig_bits ~msbit formatter =
      begin
        Range.fold (0L =:< precision) ~init:formatter ~f:(fun formatter i ->
          let msbit = Sint.(msbit + (Uns.bits_to_sint (i * bits_per_digit))) in
          formatter
          |> Fmt.fmt (
            match alt && Uns.(i > 0L) && Uns.(i % group = 0L) with
            | false -> ""
            | true -> "_"
          )
          |> Codepoint.fmt (digit ~class_ ~bits_per_digit ~m ~sig_bits ~msbit)
        )
      end in
    let nwhole_msbit ~class_ ~bits_per_digit ~e = begin
      match class_ with
      | Class.Zero -> 0L, 0L
      | Class.Normal
      | Class.Subnormal -> begin
          match Sint.(e < 0L) with
          | true -> 0L, e
          | false -> begin
              let nwhole = Uns.(((bits_of_sint e) + bits_per_digit) / bits_per_digit) in
              let msbit = Sint.(succ (e - (Uns.bits_to_sint (nwhole * bits_per_digit)))) in
              nwhole, msbit
            end
        end
      | Class.Infinite
      | Class.Nan -> not_reached ()
    end in
    let fmt_denorm ~alt ~precision ~class_ ~bits_per_digit ~group ~m_norm ~e_norm ~sig_bits_norm
        formatter = begin
      let nwhole_unrounded, msbit_unrounded = nwhole_msbit ~class_ ~bits_per_digit ~e:e_norm in
      (* Round if significant bits will be omitted. *)
      let m, sig_bits, nwhole, msbit =
        let bits_avail = Uns.bits_to_sint sig_bits_norm in
        let bits_used = Sint.(pred msbit_unrounded + Uns.(bits_to_sint ((nwhole_unrounded +
            precision) * bits_per_digit))) in
        match Sint.(bits_avail <= bits_used) with
        | true -> m_norm, sig_bits_norm, nwhole_unrounded, msbit_unrounded
        | false -> begin
            let half_ulp = (bit_sl ~shift:Uns.(pred (bits_of_sint Sint.(bits_avail - bits_used)))
              1L) in
            let m_rounded = Uns.(m_norm + half_ulp) in
            (* If rounding overflows, shift one bit out of the mantissa and correspondingly adjust
             * {nwhole,msbit}. *)
            let mask = pred (bit_sl ~shift:sig_bits_norm 1L) in
            match (bit_and m_rounded mask) = m_rounded with
            | true -> m_rounded, sig_bits_norm, nwhole_unrounded, msbit_unrounded
            | false -> begin
                let e_rounded = succ e_norm in
                let sig_bits_rounded = succ sig_bits_norm in
                let nwhole_rounded, msbit_rounded = nwhole_msbit ~class_ ~bits_per_digit
                    ~e:e_rounded in
                m_rounded, sig_bits_rounded, nwhole_rounded, msbit_rounded
              end
          end
      in
      let msbit_part = Sint.(msbit + Uns.(bits_to_sint (nwhole * bits_per_digit))) in
      formatter
      |> fmt_whole ~alt ~class_ ~bits_per_digit ~group ~m ~sig_bits ~nwhole ~msbit
      |> Fmt.fmt "."
      |> fmt_part ~alt ~precision ~class_ ~bits_per_digit ~group ~m ~msbit:msbit_part ~sig_bits
    end in
    match classify t with
    | Class.Normal
    | Class.Subnormal
    | Class.Zero as class_ -> begin
        let bits_per_digit, group = match base with
          | Fmt.Bin -> 1L, 8L
          | Fmt.Oct -> 3L, 3L
          | Fmt.Dec -> not_reached ()
          | Fmt.Hex -> 4L, 4L
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
          String.Fmt.empty
          |> fmt_sign (is_neg t) ~sign false
          |> Fmt.fmt (match base with
            | Fmt.Bin -> "0b"
            | Fmt.Oct -> "0o"
            | Fmt.Dec -> not_reached ()
            | Fmt.Hex -> "0x"
          )
          |> Fmt.to_string
        in
        let suffix =
          String.Fmt.empty
          |> (match notation with
            | Fmt.Normalized -> fmt_norm ~sign ~alt ~precision ~class_ ~bits_per_digit ~group
                ~m_norm ~e_norm ~sig_bits_norm
            | Fmt.RadixPoint -> fmt_denorm ~alt ~precision ~class_ ~bits_per_digit ~group ~m_norm
                ~e_norm ~sig_bits_norm
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

  let rec to_string ?(sign=Fmt.sign_default) ?(alt=Fmt.alt_default) ?(zpad=Fmt.zpad_default)
    ?(width=Fmt.width_default) ?(precision=Fmt.precision_default) ?(notation=Fmt.notation_default)
    ?(base=Fmt.base_default) t =
    match notation, base with
    | Fmt.Compact, _ -> begin
        let s_m = to_string ~sign ~alt ~zpad ~width ~precision ~notation:Fmt.Normalized ~base t in
        let s_a = to_string ~sign ~alt ~zpad ~width ~precision ~notation:Fmt.RadixPoint ~base t in
        match Cmp.is_lt (Uns.cmp (String.C.length s_m) (String.C.length s_a)) with
        | true -> s_m
        | false -> s_a
      end
    | _, Fmt.Dec -> to_string_e ~sign ~alt ~zpad ~width ~precision ~notation t
    | _, Fmt.Bin
    | _, Fmt.Oct
    | _, Fmt.Hex -> to_string_p ~sign ~alt ~zpad ~width ~precision ~notation ~base t

  let fmt ?pad ?just ?sign ?alt ?zpad ?width ?precision ?notation ?base t formatter =
    Fmt.fmt ?pad ?just ?width (to_string ?sign ?alt ?zpad ?width ?precision ?notation ?base t)
      formatter

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

    let xpp xppf t =
      Format.fprintf xppf "@[<h>{fractional:@ %h,@ integral:@ %h}@]"
        t.fractional t.integral

    let pp t formatter =
      formatter
      |> Fmt.fmt "{fractional: "
      |> fmt ~base:Fmt.Hex t.fractional
      |> Fmt.fmt ", integral: "
      |> fmt ~base:Fmt.Hex t.integral
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
