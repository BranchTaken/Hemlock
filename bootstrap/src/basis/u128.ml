include Rudiments_int
open Rudiments_functions

module T = struct
  module U = struct
    type t = u128

    let hash_fold t state =
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u128 1 ~f:(fun _ -> t)
      |> Hash.State.Gen.fini

    let cmp t0 t1 =
      match (U64.cmp t0.hi t1.hi), (U64.cmp t0.lo t1.lo) with
      | Lt, _
      | Eq, Lt -> Cmp.Lt
      | Eq, Eq -> Cmp.Eq
      | Gt, _
      | Eq, Gt -> Cmp.Gt

    let zero = u128_zero

    let one = u128_one
  end
  include U
  include Cmpable.Make(U)

  let of_float f =
    let hi = U64.of_float (f /. (Caml.Float.pow 2. 64.)) in
    let lo = U64.of_float f in
    {hi; lo}

  let to_float t =
    (U64.to_float t.hi) *. (Caml.Float.pow 2. 64.) +. (U64.to_float t.lo)

  let to_u64 t =
    t.lo

  let to_u64_hlt t =
    match U64.(t.hi = zero) with
    | false -> halt "Lossy conversion"
    | true -> t.lo

  let of_u64 u =
    {hi=U64.zero; lo=u}

  let to_usize t =
    U64.to_usize t.lo

  let to_usize_hlt t =
    match U64.(t.hi = zero) with
    | false -> halt "Lossy conversion"
    | true -> U64.to_usize_hlt t.lo

  let of_usize = u128_of_usize

  let min_value = zero

  let max_value = {hi=U64.max_value; lo=U64.max_value}

  let succ t =
    match U64.(t.lo = max_value) with
    | false -> {t with lo=U64.succ t.lo}
    | true -> {hi=U64.succ t.hi; lo=U64.zero}

  let pred t =
    match U64.(t.lo = zero) with
    | false -> {t with lo=U64.pred t.lo}
    | true -> {hi=U64.pred t.hi; lo=U64.max_value}

  let bit_and t0 t1 =
    {hi=U64.bit_and t0.hi t1.hi; lo=U64.bit_and t0.lo t1.lo}

  let bit_or = u128_bit_or

  let bit_xor t0 t1 =
    {hi=U64.bit_xor t0.hi t1.hi; lo=U64.bit_xor t0.lo t1.lo}

  let bit_not t =
    {hi=U64.bit_not t.hi; lo=U64.bit_not t.lo}

  let bit_sl = u128_bit_sl

  let bit_usr = u128_bit_usr

  let bit_ssr ~shift t =
    let i = shift % 128 in
    let hi = begin
      if Rudiments_int.(i >= 64) then Int64.shift_right t.hi (i - 64)
      else if Rudiments_int.(i > 0) then Int64.shift_right t.hi i
      else t.hi
    end in
    let lo = begin
      if Rudiments_int.(i >= 64) then Int64.shift_right t.hi (i - 64)
      else if Rudiments_int.(i > 0) then
        Int64.logor (Int64.shift_left t.hi (64 - i))
          (Int64.shift_right_logical t.lo i)
      else t.lo
    end in
    {hi; lo}

  let ( + ) = u128_add

  let ( - ) t0 t1 =
    let borrow = if U64.(t0.lo < t1.lo) then U64.one else U64.zero in
    let hi = U64.(t0.hi - t1.hi - borrow) in
    let lo = U64.(t0.lo - t1.lo) in
    {hi; lo}

  let ( * ) = u128_mul

  let of_string = u128_of_string
  let c5s = of_string "0x5555_5555_5555_5555_5555_5555_5555_5555"
  let c3s = of_string "0x3333_3333_3333_3333_3333_3333_3333_3333"
  let c0fs = of_string "0x0f0f_0f0f_0f0f_0f0f_0f0f_0f0f_0f0f_0f0f"
  let cff = of_string "0xff"

  let bit_pop x =
    let x =
      x - (bit_and (bit_usr ~shift:1 x) c5s) in
    let x = (bit_and x c3s) + (bit_and (bit_usr ~shift:2 x) c3s) in
    let x = bit_and (x + (bit_usr ~shift:4 x)) c0fs in
    let x = x + (bit_usr ~shift:8 x) in
    let x = x + (bit_usr ~shift:16 x) in
    let x = x + (bit_usr ~shift:32 x) in
    let x = x + (bit_usr ~shift:64 x) in
    to_usize (bit_and x cff)

  let bit_clz x =
    let x = bit_or x (bit_usr ~shift:1 x) in
    let x = bit_or x (bit_usr ~shift:2 x) in
    let x = bit_or x (bit_usr ~shift:4 x) in
    let x = bit_or x (bit_usr ~shift:8 x) in
    let x = bit_or x (bit_usr ~shift:16 x) in
    let x = bit_or x (bit_usr ~shift:32 x) in
    let x = bit_or x (bit_usr ~shift:64 x) in
    bit_pop (bit_not x)

  let bit_ctz t =
    bit_pop (bit_and (bit_not t) (t - one))

  let div_mod t0 t1 =
    (* Compute quotient and remainder using an algorithm similar to the paper
     * long division algorithm, but in base 2^32.
     *
     * The digit arrays are encoded as (u32 array), which assures that only
     * significant bits are stored.  The intermediate computations use 64-bit
     * math so that two digits fit. *)
    let b = U64.(bit_sl ~shift:32 one) in
    (* Extract the high/low digit from a 2-digit value. *)
    let hi32 x = U64.bit_usr ~shift:32 x in
    let lo32 x = U64.(bit_and x (of_usize 0xffff_ffff)) in
    let div_b x = Int64.shift_right x 32 in
    let mul_b x = I64.bit_sl ~shift:32 x in
    (* Get/set digit.  Only the low 32 bits are used; if u32 were available it
     * would be a better choice for array elements. *)
    let get arr i = U64.to_i64 (U32.to_u64 (Caml.Array.get arr i)) in
    let set arr i x = Caml.Array.set arr i (U32.of_u64 (U64.of_i64 x)) in
    (* Digit array creation and conversion functions.  Digits are in
     * little-endian order (least significant digit at offset 0). *)
    let zero_arr ndigits = Caml.Array.make ndigits U32.zero in
    let to_arr u = [|U32.of_u64 (lo32 u.lo); U32.of_u64 (hi32 u.lo);
      U32.of_u64 (lo32 u.hi); U32.of_u64 (hi32 u.hi);|] in
    let of_arr arr = begin
      let hi = U64.(bit_or (mul_b (get arr 3)) (get arr 2)) in
      let lo = U64.(bit_or (mul_b (get arr 1)) (get arr 0)) in
      {hi; lo}
    end in
    (* Compute the number of significant digits in a digit array, i.e. subtract
     * high-order zeros from the array size. *)
    let sig_digits arr = begin
      let rec fn arr past = begin
        match past with
        | 0 -> 0
        | _ -> begin
            let i = Rudiments_int.pred past in
            let digit = get arr i in
            match U64.(digit = zero) with
            | false -> past
            | true -> fn arr i
          end
      end in
      fn arr (Caml.Array.length arr)
    end in
    let u = to_arr t0 in (* Dividend. *)
    let v = to_arr t1 in (* Divisor. *)
    let q = to_arr zero in (* Quotient. *)
    let r = to_arr zero in (* Remainder. *)
    let m = sig_digits u in
    let n = sig_digits v in

    match n, Rudiments_int.(m < n) with
    | 0, _ -> halt "Division by 0"
    | _, true -> zero, t0
    | 1, _ -> begin
        (* Single-digit divisor. *)
        let rec fn_j j carry = begin
          let t = I64.(carry * b + (get u j)) in
          set q j I64.(t / (get v 0));
          let carry' = U64.(t - (get q j) * (get v 0)) in
          match Rudiments_int.(j = 0) with
          | true -> carry'
          | false -> fn_j Rudiments_int.(pred j) carry'
        end in
        let carry = fn_j Rudiments_int.(pred m) U64.zero in
        set r 0 carry;
        of_arr q, of_arr r
      end
    | _, false -> begin
        (* Choose normalization power-of-2 multiplier such that the divisor is
         * losslessly shifted left as far as possible. *)
        let shift = Rudiments_int.(
          (U64.bit_clz (get v Rudiments_int.(pred n))) - 32) in
        (* Initialize normalized divisor. *)
        let vn = zero_arr n in
        for i = Rudiments_int.(pred n) downto 1 do
          set vn i (U64.bit_or
              (U64.bit_sl ~shift (get v i))
              (U64.bit_usr ~shift:Rudiments_int.(32 - shift)
                  (get v Rudiments_int.(pred i)))
          )
        done;
        set vn 0 (U64.bit_sl ~shift (get v 0));
        (* Initialize normalized dividend. *)
        let un = zero_arr (Rudiments_int.succ m) in
        set un m (U64.bit_usr ~shift:Rudiments_int.(32 - shift)
          (get u (Rudiments_int.pred m)));
        for i = Rudiments_int.(pred m) downto 1 do
          set un i U64.(bit_or
              U64.(bit_sl ~shift (get u i))
              U64.(bit_usr ~shift:Rudiments_int.(32 - shift)
                (get u (Rudiments_int.pred i)))
          )
        done;
        set un 0 (U64.bit_sl ~shift (get u 0));
        (* Main computation. *)
        let rec fn_j j = begin
          (* Compute quotient digit estimate and remainder.  It is possible that
           * qdigit is one larger than the correct value, in which case
           * subsequent correction code executes. *)
          let qdigit = begin
            let rec qdigit_converge qdigit rem = begin
              match (U64.(qdigit >= b) ||
                     U64.((qdigit * (get vn Rudiments_int.(n - 2))) >
                       (b * rem + (get un Rudiments_int.(j + n - 2))))) with
              | false -> qdigit
              | true -> begin
                  let qdigit' = U64.pred qdigit in
                  let rem' = U64.(rem + (get vn (Rudiments_int.pred n))) in
                  match U64.(rem' < b) with
                  | true -> qdigit_converge qdigit' rem'
                  | false -> qdigit'
                end
            end in
            let t = U64.(((get un Rudiments_int.(j + n)) * b +
                (get un Rudiments_int.(j + n - 1)))) in
            let qdigit_est = U64.(t / (get vn (Rudiments_int.pred n))) in
            let rem = U64.(t - qdigit_est * (get vn (Rudiments_int.pred n))) in
            qdigit_converge qdigit_est rem
          end in
          (* Multiply and subtract. *)
          let rec fn_i i carry = begin
            match Rudiments_int.(i = n) with
            | true -> carry
            | false -> begin
                let p = U64.(qdigit * (get vn i)) in
                let t =
                  I64.((get un Rudiments_int.(i + j)) - carry - (lo32 p)) in
                set un Rudiments_int.(i + j) t;
                let carry' = I64.((div_b p) - (div_b t)) in
                fn_i (Rudiments_int.succ i) carry'
              end
          end in
          let carry = fn_i 0 I64.zero in
          let t = I64.((get un Rudiments_int.(j + n)) - carry) in
          set un Rudiments_int.(j + n) t;
          (* Store quotient digit. *)
          match I64.(t < zero) with
          | false -> set q j qdigit
          | true -> begin
              (* Subtracted too much; correct. *)
              set q j (U64.pred qdigit);
              let rec fn_i i carry = begin
                match Rudiments_int.(i = n) with
                | true -> carry
                | false -> begin
                    let t = I64.((get un Rudiments_int.(i + j)) + (get vn i)
                      + carry) in
                    set un Rudiments_int.(i + j) t;
                    let carry' = div_b t in
                    fn_i (Rudiments_int.succ i) carry'
                  end
              end in
              let carry = fn_i 0 I64.zero in
              set un Rudiments_int.(j + n)
                I64.((get un Rudiments_int.(j + n)) + carry)
            end
            ;
            match Rudiments_int.(j = 0) with
            | true -> ()
            | false -> fn_j (Rudiments_int.pred j)
        end in
        fn_j Rudiments_int.(m - n);
        (* Denormalize remainder. *)
        let i_last = Rudiments_int.(pred n) in
        assert Rudiments_int.(i_last > 0);
        for i = 0 to Rudiments_int.(pred i_last) do
          set r i U64.(bit_or
              (bit_usr ~shift (get un i))
              (bit_sl ~shift:Rudiments_int.(32 - shift)
                  (get un (Rudiments_int.succ i))))
        done;
        set r i_last U64.(bit_or
            (bit_usr ~shift (get un i_last))
            (bit_sl ~shift:Rudiments_int.(32 - shift)
                (get un (Rudiments_int.succ i_last))));
        of_arr q, of_arr r
      end

  let ( / ) t0 t1 =
    let quotient, _ = div_mod t0 t1 in
    quotient

  let ( % ) t0 t1 =
    let _, remainder = div_mod t0 t1 in
    remainder

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
    (to_float t0) /. (to_float t1)

  module V = struct
    type nonrec t = t

    let num_bits = 128

    let cmp = cmp
    let zero = zero
    let one = one
    let of_usize = of_usize
    let ( + ) = ( + )
    let ( - ) = ( - )
    let bit_and = bit_and
    let bit_sl = bit_sl
    let bit_clz = bit_clz
  end
  include Intnb.Make_derived(V)

  let pp ppf t =
    let rec fn t = begin
      match cmp t zero with
      | Cmp.Eq -> 0
      | Cmp.Lt | Cmp.Gt -> begin
          let t' = t / (of_usize 10) in
          let i = fn t' in
          let digit = t % (of_usize 10) in
          if Rudiments_int.(i % 3 = 0) && Rudiments_int.(i > 0) then
            Format.fprintf ppf "_";
          Format.fprintf ppf "%Lu" digit.lo;
          Rudiments_int.(succ i)
        end
    end in
    match t = zero with
    | true -> Format.fprintf ppf "0u128"
    | false -> begin
        let _ = fn t in
        Format.fprintf ppf "u128"
      end

  let pp_x = u128_pp_x

  let to_string t =
    Format.asprintf "%a" pp t
end
include T
include Identifiable.Make(T)
include Cmpable.Make_zero(T)

(******************************************************************************)
(* Begin tests. *)

let%expect_test "pp,pp_x" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        printf "%a %a\n" pp x pp_x x;
        fn xs'
      end
  in
  printf "@[<h>";
  fn [
    zero;
    one;
    of_string "42";
    min_value;
    max_value
  ];
  printf "@]";

  [%expect{|
    0u128 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    1u128 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    42u128 0x0000_0000_0000_0000_0000_0000_0000_002au128
    0u128 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    340_282_366_920_938_463_463_374_607_431_768_211_455u128 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    |}]

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x u Hash.pp (Hash.t_of_state (hash_fold u Hash.State.empty));
        test_hash_fold us'
      end
  end in
  let us = [zero; one; min_value; max_value] in
  test_hash_fold us;
  printf "@]";

  [%expect{|
    hash_fold 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0xd0ac_179d_2650_b632_d8ce_73d7_a39a_46e6u128
    |}]

let%expect_test "constants" =
  let open Format in

  printf "zero=%a\n" pp_x zero;
  printf "one=%a\n" pp_x one;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    zero=0x0000_0000_0000_0000_0000_0000_0000_0000u128
    one=0x0000_0000_0000_0000_0000_0000_0000_0001u128
    min_value=0x0000_0000_0000_0000_0000_0000_0000_0000u128
    max_value=0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    |}]

let%expect_test "of_string" =
  let open Format in
  printf "@[<h>";
  let rec test_strs = function
    | [] -> ()
    | s :: strs' -> begin
        printf "of_string %S -> %a\n" s pp_x (of_string s);
        test_strs strs'
      end
  in
  let strs = [
    "0";
    "1";
    "9876543210";
    "9876543210_";
    "9u128";
    "9_u128";
    "340282366920938463463374607431768211455";

    "0b0";
    "0b1";
    "0b10";
    "0b10_";
    "0b1u128";
    "0b_1_u128";
    ("0b_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111");

    "0x0";
    "0x1";
    "0xfedcba9876543210";
    "0xfedcba9876543210_";
    "0xfu128";
    "0x_f_u128";
    "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";
  ] in
  test_strs strs;
  printf "@]";

  [%expect{|
    of_string "0" -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    of_string "1" -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    of_string "9876543210" -> 0x0000_0000_0000_0000_0000_0002_4cb0_16eau128
    of_string "9876543210_" -> 0x0000_0000_0000_0000_0000_0002_4cb0_16eau128
    of_string "9u128" -> 0x0000_0000_0000_0000_0000_0000_0000_0009u128
    of_string "9_u128" -> 0x0000_0000_0000_0000_0000_0000_0000_0009u128
    of_string "340282366920938463463374607431768211455" -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    of_string "0b0" -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    of_string "0b1" -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    of_string "0b10" -> 0x0000_0000_0000_0000_0000_0000_0000_0002u128
    of_string "0b10_" -> 0x0000_0000_0000_0000_0000_0000_0000_0002u128
    of_string "0b1u128" -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    of_string "0b_1_u128" -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    of_string "0b_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111" -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    of_string "0x0" -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    of_string "0x1" -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    of_string "0xfedcba9876543210" -> 0x0000_0000_0000_0000_fedc_ba98_7654_3210u128
    of_string "0xfedcba9876543210_" -> 0x0000_0000_0000_0000_fedc_ba98_7654_3210u128
    of_string "0xfu128" -> 0x0000_0000_0000_0000_0000_0000_0000_000fu128
    of_string "0x_f_u128" -> 0x0000_0000_0000_0000_0000_0000_0000_000fu128
    of_string "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff" -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    |}]

let%expect_test "rel" =
  let open Format in
  let fn x y = begin
    printf "cmp %a %a -> %a\n" pp_x x pp_x y Cmp.pp (cmp x y);
    printf "%a >= %a -> %b\n" pp_x x pp_x y (x >= y);
    printf "%a <= %a -> %b\n" pp_x x pp_x y (x <= y);
    printf "%a = %a -> %b\n" pp_x x pp_x y (x = y);
    printf "%a > %a -> %b\n" pp_x x pp_x y (x > y);
    printf "%a < %a -> %b\n" pp_x x pp_x y (x < y);
    printf "%a <> %a -> %b\n" pp_x x pp_x y (x <> y);
    printf "ascending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (ascending x y);
    printf "descending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (descending x y);
  end in
  fn zero (of_string "0x8000_0000_0000_0000");
  printf "\n";
  fn zero (of_string "0xffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (of_string "0x7fff_ffff_ffff_fffe") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x7fff_ffff_ffff_ffff") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0001") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0002") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");

  [%expect{|
    cmp 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> Lt
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 >= 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <= 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 = 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 > 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 < 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <> 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> true
    ascending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> Lt
    descending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> Gt

    cmp 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> Lt
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 >= 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <= 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 = 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 > 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 < 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> true
    ascending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> Lt
    descending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> Gt

    cmp 0x0000_0000_0000_0000_8000_0000_0000_0000u128 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> Gt
    0x0000_0000_0000_0000_8000_0000_0000_0000u128 >= 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> true
    0x0000_0000_0000_0000_8000_0000_0000_0000u128 <= 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_8000_0000_0000_0000u128 = 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_8000_0000_0000_0000u128 > 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> true
    0x0000_0000_0000_0000_8000_0000_0000_0000u128 < 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_8000_0000_0000_0000u128 <> 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> true
    ascending 0x0000_0000_0000_0000_8000_0000_0000_0000u128 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> Gt
    descending 0x0000_0000_0000_0000_8000_0000_0000_0000u128 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> Lt

    clamp ~min:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~max:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_7fff_ffff_ffff_fffeu128 -> 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128
    between ~low:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~high:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_7fff_ffff_ffff_fffeu128 -> false

    clamp ~min:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~max:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128
    between ~low:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~high:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 -> true

    clamp ~min:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~max:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_8000_0000_0000_0000u128
    between ~low:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~high:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_8000_0000_0000_0000u128 -> true

    clamp ~min:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~max:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_8000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_8000_0000_0000_0001u128
    between ~low:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~high:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_8000_0000_0000_0001u128 -> true

    clamp ~min:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~max:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_8000_0000_0000_0002u128 -> 0x0000_0000_0000_0000_8000_0000_0000_0001u128
    between ~low:0x0000_0000_0000_0000_7fff_ffff_ffff_ffffu128 ~high:0x0000_0000_0000_0000_8000_0000_0000_0001u128 0x0000_0000_0000_0000_8000_0000_0000_0002u128 -> false
    |}]

let%expect_test "narrowing" =
  let open Format in
  let fifteen = of_string "15" in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    min_value - 1u128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    max_value * 15u128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fff1u128
    |}]

let%expect_test "+,-" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a +,- %a -> %a, %a\n" pp_x x pp_x y pp_x (x + y) pp_x (x - y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");

    (of_string "0", of_string "1");
    (of_string "1", of_string "0");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 +,- 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    0x0000_0000_0000_0000_0000_0000_0000_0001u128 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    0x0000_0000_0000_0000_0000_0000_0000_0001u128 +,- 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0001_0000_0000_0000_0000u128, 0xffff_ffff_ffff_ffff_0000_0000_0000_0002u128
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 +,- 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0001_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_ffff_ffff_ffff_fffeu128
    0x0000_0000_0000_0000_0000_0000_0000_0001u128 +,- 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0002u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 +,- 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffeu128
    |}]

let%expect_test "*" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let z = x * y in
        printf "%a * %a -> %a\n" pp_x x pp_x y pp_x z;
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 * 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 * 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0001u128 * 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    0x0000_0000_0000_0000_0000_0000_ffff_ffffu128 * 0x0000_0000_0000_0000_0000_0000_ffff_ffffu128 -> 0x0000_0000_0000_0000_ffff_fffe_0000_0001u128
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 * 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> 0xffff_ffff_ffff_fffe_0000_0000_0000_0001u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 * 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    |}]

let%expect_test "of_float,to_float" =
  let open Format in
  printf "@[<h>";
  let rec test_fs fs = begin
    match fs with
    | [] -> ()
    | f :: fs' -> begin
        let x = of_float f in
        printf "of_float %h -> %a; to_float -> %h\n"
          f pp_x x (to_float x);
        test_fs fs'
      end
  end in
  let fs = [
    -1.;
    0.;
    0x1.1p-1;
    1.;

    0x1.f_ffff_ffff_ffffp48;
    0x1.f_ffff_ffff_ffffp52;
    0x1.f_ffff_ffff_ffffp56;

    0x1.f_ffff_ffff_ffffp127;
    0x1.f_ffff_ffff_ffffp128;
    0x1.f_ffff_ffff_ffffp132;

    0x1p126;
    0x1p127;
    0x1p128;
  ] in
  test_fs fs;
  printf "\n";
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let f = to_float x in
        printf "to_float %a -> %h; of_float -> %a\n"
          pp_x x f pp_x (of_float f);
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
    max_value;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_float -0x1p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_float -> 0x0p+0
    of_float 0x0p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0x0000_0000_0000_0000_0001_ffff_ffff_ffffu128; to_float -> 0x1.ffffffffffffp+48
    of_float 0x1.fffffffffffffp+52 -> 0x0000_0000_0000_0000_001f_ffff_ffff_ffffu128; to_float -> 0x1.fffffffffffffp+52
    of_float 0x1.fffffffffffffp+56 -> 0x0000_0000_0000_0000_01ff_ffff_ffff_fff0u128; to_float -> 0x1.fffffffffffffp+56
    of_float 0x1.fffffffffffffp+127 -> 0xffff_ffff_ffff_f800_0000_0000_0000_0000u128; to_float -> 0x1.fffffffffffffp+127
    of_float 0x1.fffffffffffffp+128 -> 0xffff_ffff_ffff_f000_0000_0000_0000_0000u128; to_float -> 0x1.ffffffffffffep+127
    of_float 0x1.fffffffffffffp+132 -> 0xffff_ffff_ffff_0000_0000_0000_0000_0000u128; to_float -> 0x1.fffffffffffep+127
    of_float 0x1p+126 -> 0x4000_0000_0000_0000_0000_0000_0000_0000u128; to_float -> 0x1p+126
    of_float 0x1p+127 -> 0x8000_0000_0000_0000_0000_0000_0000_0000u128; to_float -> 0x1p+127
    of_float 0x1p+128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_float -> 0x0p+0

    to_float 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0p+0; of_float -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    to_float 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x1p+0; of_float -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    to_float 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x1.fffffffffffffp+127; of_float -> 0xffff_ffff_ffff_f800_0000_0000_0000_0000u128
    |}]

let%expect_test "/,%" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let quotient = x / y in
        let remainder = x % y in
        printf "%a /,%% %a -> %a, %a\n"
          pp_x x pp_x y pp_x quotient pp_x remainder;
        assert (x = (y * quotient + remainder));
        test_pairs pairs'
      end
  in
  let pairs = [
    (* < 1 *)
    (of_string "0", of_string "1");
    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (of_string "0xfffe", of_string "0xffff");
    (of_string "0xffff_fffe", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_fffe", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xf_ffff_ffff_ffff_fffe", of_string "0xf_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (* Single-digit (base 2^32) divisor. *)
    (of_string "1", of_string "1");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "2");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "3");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "7");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "0xffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff");

    (* Multi-digit (base 2^32) divisor. *)
    (of_string "0x1_0000_0000", of_string "0x1_0000_0000");
    (of_string "0x1_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0x2_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 /,% 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0001u128 /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    0x0000_0000_0000_0000_0000_0000_0000_fffeu128 /,% 0x0000_0000_0000_0000_0000_0000_0000_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_fffeu128
    0x0000_0000_0000_0000_0000_0000_ffff_fffeu128 /,% 0x0000_0000_0000_0000_0000_0000_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_ffff_fffeu128
    0x0000_0000_0000_0000_ffff_ffff_ffff_fffeu128 /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_ffff_ffff_ffff_fffeu128
    0x0000_0000_0000_000f_ffff_ffff_ffff_fffeu128 /,% 0x0000_0000_0000_000f_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_000f_ffff_ffff_ffff_fffeu128
    0x0000_0000_0000_ffff_ffff_ffff_ffff_fffeu128 /,% 0x0000_0000_0000_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_ffff_ffff_ffff_ffff_fffeu128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffeu128 /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffeu128
    0x0000_0000_0000_0000_0000_0000_0000_0001u128 /,% 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0000_0000_0002u128 -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128, 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0000_0000_0003u128 -> 0x5555_5555_5555_5555_5555_5555_5555_5555u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0000_0000_0007u128 -> 0x2492_4924_9249_2492_4924_9249_2492_4924u128, 0x0000_0000_0000_0000_0000_0000_0000_0003u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0000_0000_ffffu128 -> 0x0001_0001_0001_0001_0001_0001_0001_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0000_ffff_ffffu128 -> 0x0000_0001_0000_0001_0000_0001_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0001_0000_0000u128 /,% 0x0000_0000_0000_0000_0000_0001_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0001_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0001_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_ffff_ffffu128
    0x0000_0000_0000_0000_0000_0002_ffff_ffffu128 /,% 0x0000_0000_0000_0000_0000_0001_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0002u128, 0x0000_0000_0000_0000_0000_0000_ffff_ffffu128
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0001_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128
    |}]

let%expect_test "bit_and,bit_or,bit_xor" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "bit_{and,or,xor} %a %a -> %a, %a, %a\n"
          pp_x x pp_x y
          pp_x (bit_and x y)
          pp_x (bit_or x y)
          pp_x (bit_xor x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "0");
    (of_string "0", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    bit_{and,or,xor} 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    bit_{and,or,xor} 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    bit_{and,or,xor} 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    |}]

let%expect_test "bit_not" =
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_not %a -> %a\n"
          pp_x x pp_x (bit_not x);
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    bit_not 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    |}]

let%expect_test "bit_pop,bit_clz,bit_ctz" =
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_{pop,clz,ctz} %a -> %u, %u, %u\n"
          pp_x x (bit_pop x) (bit_clz x) (bit_ctz x);
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0, 128, 128
    bit_{pop,clz,ctz} 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 1, 127, 0
    bit_{pop,clz,ctz} 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> 1, 0, 127
    bit_{pop,clz,ctz} 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 128, 0, 0
    |}]

let%expect_test "**" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a ** %a -> %a\n" pp_x x pp_x y pp_x (x ** y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");

    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "0");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "2", of_string "31");
    (of_string "2", of_string "32");
    (of_string "2", of_string "63");
    (of_string "2", of_string "64");
    (of_string "2", of_string "127");
    (of_string "2", of_string "128");

    (of_string "0xf", of_string "0xf");
    (of_string "0xff", of_string "0xff");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 ** 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 ** 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ** 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ** 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    0x0000_0000_0000_0000_0000_0000_0000_0002u128 ** 0x0000_0000_0000_0000_0000_0000_0000_001fu128 -> 0x0000_0000_0000_0000_0000_0000_8000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0002u128 ** 0x0000_0000_0000_0000_0000_0000_0000_0020u128 -> 0x0000_0000_0000_0000_0000_0001_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0002u128 ** 0x0000_0000_0000_0000_0000_0000_0000_003fu128 -> 0x0000_0000_0000_0000_8000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0002u128 ** 0x0000_0000_0000_0000_0000_0000_0000_0040u128 -> 0x0000_0000_0000_0001_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0002u128 ** 0x0000_0000_0000_0000_0000_0000_0000_007fu128 -> 0x8000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_0002u128 ** 0x0000_0000_0000_0000_0000_0000_0000_0080u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    0x0000_0000_0000_0000_0000_0000_0000_000fu128 ** 0x0000_0000_0000_0000_0000_0000_0000_000fu128 -> 0x0000_0000_0000_0000_0613_b62c_5977_07efu128
    0x0000_0000_0000_0000_0000_0000_0000_00ffu128 ** 0x0000_0000_0000_0000_0000_0000_0000_00ffu128 -> 0xee62_0a94_faa4_2c39_5997_756b_007f_feffu128
    0x0000_0000_0000_0000_0000_0000_0000_0001u128 ** 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ** 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    |}]

let%expect_test "is_pow2" =
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "is_pow2 %a -> %b\n"
          pp_x u
          (is_pow2 u);
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> false
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> true
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0002u128 -> true
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0003u128 -> false
    is_pow2 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> true
    is_pow2 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> false
    |}]

let%expect_test "floor_pow2,ceil_pow2" =
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_pow2,ceil_pow2 %a -> %a, %a\n"
          pp_x u
          pp_x (floor_pow2 u)
          pp_x (ceil_pow2 u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_0000_0000_0000_0002u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0002u128, 0x0000_0000_0000_0000_0000_0000_0000_0002u128
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_0000_0000_0000_0003u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0002u128, 0x0000_0000_0000_0000_0000_0000_0000_0004u128
    floor_pow2,ceil_pow2 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x8000_0000_0000_0000_0000_0000_0000_0000u128, 0x8000_0000_0000_0000_0000_0000_0000_0000u128
    floor_pow2,ceil_pow2 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x8000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    |}]

let%expect_test "floor_lg,ceil_lg" =
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_lg,ceil_lg %a -> %a, %a\n"
          pp_x u
          pp (floor_lg u)
          pp (ceil_lg u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
        floor_lg,ceil_lg 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0u128, 0u128
        floor_lg,ceil_lg 0x0000_0000_0000_0000_0000_0000_0000_0002u128 -> 1u128, 1u128
        floor_lg,ceil_lg 0x0000_0000_0000_0000_0000_0000_0000_0003u128 -> 1u128, 2u128
        floor_lg,ceil_lg 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> 127u128, 127u128
        floor_lg,ceil_lg 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 127u128, 128u128
    |}]

let%expect_test "min,max" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "min,max %a %a -> %a, %a\n"
          pp_x x pp_x y pp_x (min x y) pp_x (max x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "0");
    (of_string "1", of_string "1");
    (of_string "0", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0001u128 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0001u128 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
   |}]
