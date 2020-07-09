open Rudiments_functions

type i64 = int64
type u64 = int64
type u128 = {
  hi: u64;
  lo: u64;
}
type isize = int
type usize = int

let usize_of_isize t =
  t

let isize_of_usize t =
  t

let int_of_isize t =
  t

let isize_of_int t =
  t

let u128_of_usize u =
  {hi=Int64.zero; lo=Int64.of_int u}

let u128_pp_x ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if shift < 64 then Format.fprintf ppf "_";
        let shift' = shift - 16 in
        Format.fprintf ppf "%04Lx"
          Int64.(logand (shift_right_logical x shift') (of_int 0xffff));
        fn x shift'
      end
  end in
  Format.fprintf ppf "0x";
  fn t.hi 64;
  Format.fprintf ppf "_";
  fn t.lo 64;
  Format.fprintf ppf "u128"

let u128_zero = {hi=Int64.zero; lo=Int64.zero}

let u128_one = {hi=Int64.zero; lo=Int64.one}

let u128_bit_or t0 t1 =
  {hi=Int64.logor t0.hi t1.hi; lo=Int64.logor t0.lo t1.lo}

let u128_bit_sl ~shift t =
  let i = shift mod 128 in
  let hi = begin
    if i >= 64 then Int64.shift_left t.lo (i - 64)
    else if i > 0 then
      Int64.logor (Int64.shift_left t.hi i)
        (Int64.shift_right_logical t.lo (64 - i))
    else t.hi
  end in
  let lo = begin
    if i >= 64 then Int64.zero
    else if i > 0 then Int64.shift_left t.lo i
    else t.lo
  end in
  {hi; lo}

let u128_bit_usr ~shift t =
  let i = shift mod 128 in
  let hi = begin
    if i >= 64 then Int64.zero
    else if i > 0 then Int64.shift_right_logical t.hi i
    else t.hi
  end in
  let lo = begin
    if i >= 64 then Int64.shift_right_logical t.hi (i - 64)
    else if i > 0 then
      Int64.logor (Int64.shift_left t.hi (64 - i))
        (Int64.shift_right_logical t.lo i)
    else t.lo
  end in
  {hi; lo}

let u128_add t0 t1 =
  let lo = Int64.add t0.lo t1.lo in
  let carry =
    if (Int64.unsigned_compare lo t0.lo) < 0 then Int64.one
    else Int64.zero
  in
  let hi = Int64.(add (add t0.hi t1.hi) carry) in
  {hi; lo}

let u128_mul t0 t1 =
  (* Decompose inputs into arrays of 32-bit half-words, then use the standard
   * paper method of multi-digit multiplication, but in base 2^32.  The full
   * result requires m + n digits, where m and n are the number of input digits
   * in the muliplier and multiplicand.  For this function, ndigits=m=n=4, and
   * we only calculate/preserve the lowest ndigits digits.
   *
   * The digit arrays are encoded as (u32 array), which assures that only
   * significant bits are stored.  The intermediate computations use 64-bit math
   * so that two digits fit. *)
  let hi32 x = Int64.shift_right_logical x 32 in
  let lo32 x = Int64.(logand x (of_int 0xffff_ffff)) in
  let digits32 x = hi32 x, lo32 x in
  let get arr i = Caml.Array.get arr i in
  let set arr i x =
    Caml.Array.set arr i Int64.(logand x (of_int 0xffff_ffff)) in
  let to_arr u = [|lo32 u.lo; hi32 u.lo; lo32 u.hi; hi32 u.hi;|] in
  let of_arr arr = begin
    let hi = Int64.(logor (shift_left (get arr 3) 32) (get arr 2)) in
    let lo = Int64.(logor (shift_left (get arr 1) 32) (get arr 0)) in
    {hi; lo}
  end in
  let t0_arr = to_arr t0 in
  let t1_arr = to_arr t1 in
  let product = to_arr u128_zero in
  let ndigits = 4 in
  let rec fn_i i j x = begin
    match i + j = ndigits with
    | true -> ()
    | false -> begin
        let x', d = digits32 Int64.(add (mul (get t0_arr i) (get t1_arr j))
          (add (get product (i + j)) x)) in
        set product (i + j) d;
        fn_i (i + 1) j x'
      end
  end in
  let rec fn_j j = begin
    match j = ndigits with
    | true -> ()
    | false -> begin
        fn_i 0 j Int64.zero;
        fn_j (j + 1)
      end
  end in
  fn_j 0;
  of_arr product

let u128_of_string s =
  let getc_opt s i len = begin
    match i < len with
    | false -> None
    | true -> Some ((Caml.String.get s i), (i + 1))
  end in
  let getc s i len = begin
    match getc_opt s i len with
    | None -> halt "Malformed string"
    | Some (c, i') -> c, i'
  end in
  let d_of_c c = begin
    match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      u128_of_usize ((Caml.Char.code c) - (Caml.Char.code '0'))
    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' ->
      u128_of_usize (10 + (Caml.Char.code c) - (Caml.Char.code 'a'))
    | _ -> not_reached ()
  end in
  let rec suffix s i j len = begin
    let c, i' = getc s i len in
    match c, j with
    | '1', 1
    | '2', 2 -> suffix s i' (j + 1) len
    | '8', 3 -> begin
        match i' < len with
        | true -> halt "Malformed string"
        | false -> i'
      end
    | _ -> halt "Malformed string"
  end in
  let rec hexadecimal s i ndigits len = begin
    match getc_opt s i len with
    | None -> begin
        match ndigits with
        | 0 -> halt "Malformed string" (* "0x" *)
        | _ -> u128_zero, u128_one
      end
    | Some (c, i') -> begin
        match c with
        | '_' -> hexadecimal s i' ndigits len
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> begin
            let ndigits' = ndigits + 1 in
            let accum, mult = hexadecimal s i' ndigits' len in
            let accum' = u128_add accum (u128_mul mult  (d_of_c c)) in
            let mult' = u128_mul mult (u128_of_usize 16) in
            accum', mult'
          end
        | 'u' -> begin
            let i'' = suffix s i' 1 len in
            hexadecimal s i'' ndigits len
          end
        | _ -> halt "Malformed string"
      end
  end in
  let rec decimal s i len = begin
    match getc_opt s i len with
    | None -> u128_zero, u128_one
    | Some (c, i') -> begin
        match c with
        | '_' -> decimal s i' len
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> begin
            let accum, mult = decimal s i' len in
            let accum' = u128_add accum (u128_mul mult (d_of_c c)) in
            let mult' = u128_mul mult (u128_of_usize 10) in
            accum', mult'
          end
        | 'u' -> begin
            let i'' = suffix s i' 1 len in
            decimal s i'' len
          end
        | _ -> halt "Malformed string"
      end
  end in
  let rec binary s i ndigits len = begin
    match getc_opt s i len with
    | None -> begin
        match ndigits with
        | 0 -> halt "Malformed string" (* "0b" *)
        | _ -> u128_zero, u128_one
      end
    | Some (c, i') -> begin
        match c with
        | '_' -> binary s i' ndigits len
        | '0' | '1' -> begin
            let ndigits' = ndigits + 1 in
            let accum, mult = binary s i' ndigits' len in
            let accum' = u128_add accum (u128_mul mult (d_of_c c)) in
            let mult' = u128_mul mult (u128_of_usize 2) in
            accum', mult'
          end
        | 'u' -> begin
            let i'' = suffix s i' 1 len in
            binary s i'' ndigits len
          end
        | _ -> halt "Malformed string"
      end
  end in
  let prefix1 s i len = begin
    match getc_opt s i len with
    | None -> u128_zero
    | Some (c, i') -> begin
        match c with
        | 'b' -> begin
            let accum, _ = binary s i' 0 len in
            accum
          end
        | 'x' -> begin
            let accum, _ = hexadecimal s i' 0 len in
            accum
          end
        | '_' -> begin
            let accum, _ = decimal s i' len in
            accum
          end
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
          let accum, mult = decimal s i' len in
          u128_add accum (u128_mul mult (d_of_c c))
        | 'u' -> begin
            let _ = suffix s i' 1 len in
            u128_zero
          end
        | _ -> halt "Malformed string"
      end
  end in
  let prefix0 s i len = begin
    let c, i' = getc s i len in
    match c with
    | '0' -> prefix1 s i' len
    | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> begin
        let accum, mult = decimal s i' len in
        u128_add accum (u128_mul mult (d_of_c c))
      end
    | _ -> halt "Malformed string"
  end in
  prefix0 s 0 (Caml.String.length s)
