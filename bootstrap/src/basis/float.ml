open Rudiments_int
open Rudiments0

module T = struct
  type t = float

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u128 1
      ~f:(fun _ -> u128_of_arr [|Int64.bits_of_float t; Int64.zero|])
    |> Hash.State.Gen.fini

  let cmp t0 t1 =
    let rel = Sint.of_int (compare t0 t1) in
    if Sint.(rel < (kv 0)) then
      Cmp.Lt
    else if Sint.(rel = (kv 0)) then
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
include Cmpable.Make_zero(T)

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
  assert Sint.(exponent >= (kv (-1023)));
  assert Sint.(exponent <= (kv 1024));
  assert Uns.(mantissa <= 0xf_ffff_ffff_ffff);
  let sign = match neg with
    | false -> Int64.zero
    | true -> Int64.one
  in
  let biased_exponent =
    Int64.of_int (int_of_sint Sint.(exponent + (kv 1023))) in
  let bits =
    Int64.logor (Int64.shift_left sign 63)
      (Int64.logor (Int64.shift_left biased_exponent 52)
          (Int64.of_int mantissa))
  in
  Int64.float_of_bits bits

let is_neg t =
  let bits = Int64.bits_of_float t in
  let sign = Int64.shift_right_logical bits 63 in
  Int64.equal sign Int64.one

let exponent t =
  let bits = Int64.bits_of_float t in
  let biased_exponent = sint_of_int (Int64.to_int (
    Int64.logand (Int64.shift_right_logical bits 52)
      (Int64.of_int 0x7ff))) in
  Sint.(biased_exponent - (kv 1023))

let mantissa t =
  let bits = Int64.bits_of_float t in
  Int64.to_int (Int64.logand bits (Int64.of_int 0xf_ffff_ffff_ffff))

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
    | 0 -> r
    | _ -> begin
        let r' = match Uns.bit_and n 1 with
          | 0 -> r
          | 1 -> r * p
          | _ -> not_reached ()
        in
        let p' = p * p in
        let n' = Uns.bit_usr ~shift:1 n in
        fn r' p' n'
      end
  end in
  let r = fn 1. t (Uns.of_sint n) in
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
  let g = f + (t' - 0.5) * (ln t') - t' + 0.918938533204673 +
      (((-0.000595238095238 * z + 0.000793650793651) * z -
            0.002777777777778) * z + 0.083333333333333) / t'
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

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold floats = begin
    match floats with
    | [] -> ()
    | x :: floats' -> begin
        printf "hash_fold %h -> %a\n"
          x Hash.pp (Hash.t_of_state (hash_fold x Hash.State.empty));
        test_hash_fold floats'
      end
  end in
  let floats = [0.; 1.; 42.; infinity] in
  test_hash_fold floats;
  printf "@]";

  [%expect{|
    hash_fold 0x0p+0 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x1p+0 -> 0x96b9_b9e8_7688_bee1_f1af_8bd0_2aa3_2f56u128
    hash_fold 0x1.5p+5 -> 0xcbb6_698f_134f_7e07_fafb_a2c9_4442_61edu128
    hash_fold infinity -> 0x23c7_6490_6a30_242b_61cc_57d4_7f64_339du128
    |}]

let%expect_test "create" =
  let open Format in
  printf "@[<h>";
  let rec fn tups = begin
    match tups with
    | [] -> ()
    | (n, e, m) :: tups' -> begin
        let f = create ~neg:n ~exponent:e ~mantissa:m in
        printf "n=%b, e=%a, m=%a -> %h -> n=%b, e=%a, m=%a\n"
          n Sint.pp e Uns.pp_x m f n Sint.pp e Uns.pp_x m;
        fn tups'
      end
  end in
  fn [
    (* Infinite. *)
    (true, Sint.kv 1024, 0);
    (false, Sint.kv 1024, 0);

    (* Nan. *)
    (false, Sint.kv 1024, 1);
    (false, Sint.kv 1024, 0x8_0000_0000_0001);
    (false, Sint.kv 1024, 0xf_ffff_ffff_ffff);

    (* Normal. *)
    (true, Sint.kv 0, 0);
    (false, Sint.kv (-1022), 0);
    (false, Sint.kv (-52), 1);
    (false, Sint.kv (-51), 1);
    (false, Sint.kv (-1), 0);
    (false, Sint.kv 0, 0);
    (false, Sint.kv 1, 0);
    (false, Sint.kv 1, 0x8_0000_0000_0000);
    (false, Sint.kv 2, 0);
    (false, Sint.kv 2, 0x4_0000_0000_0000);
    (false, Sint.kv 1023, 0xf_ffff_ffff_ffff);

    (* Subnormal. *)
    (false, Sint.kv (-1023), 1);
    (false, Sint.kv (-1023), 0xf_ffff_ffff_ffff);

    (* Zero. *)
    (true, Sint.kv (-1023), 0);
    (false, Sint.kv (-1023), 0);
  ];
  printf "@]";

  [%expect{|
    n=true, e=1024i, m=0x0000000000000000 -> -infinity -> n=true, e=1024i, m=0x0000000000000000
    n=false, e=1024i, m=0x0000000000000000 -> infinity -> n=false, e=1024i, m=0x0000000000000000
    n=false, e=1024i, m=0x0000000000000001 -> nan -> n=false, e=1024i, m=0x0000000000000001
    n=false, e=1024i, m=0x0008000000000001 -> nan -> n=false, e=1024i, m=0x0008000000000001
    n=false, e=1024i, m=0x000fffffffffffff -> nan -> n=false, e=1024i, m=0x000fffffffffffff
    n=true, e=0i, m=0x0000000000000000 -> -0x1p+0 -> n=true, e=0i, m=0x0000000000000000
    n=false, e=-1022i, m=0x0000000000000000 -> 0x1p-1022 -> n=false, e=-1022i, m=0x0000000000000000
    n=false, e=-52i, m=0x0000000000000001 -> 0x1.0000000000001p-52 -> n=false, e=-52i, m=0x0000000000000001
    n=false, e=-51i, m=0x0000000000000001 -> 0x1.0000000000001p-51 -> n=false, e=-51i, m=0x0000000000000001
    n=false, e=-1i, m=0x0000000000000000 -> 0x1p-1 -> n=false, e=-1i, m=0x0000000000000000
    n=false, e=0i, m=0x0000000000000000 -> 0x1p+0 -> n=false, e=0i, m=0x0000000000000000
    n=false, e=1i, m=0x0000000000000000 -> 0x1p+1 -> n=false, e=1i, m=0x0000000000000000
    n=false, e=1i, m=0x0008000000000000 -> 0x1.8p+1 -> n=false, e=1i, m=0x0008000000000000
    n=false, e=2i, m=0x0000000000000000 -> 0x1p+2 -> n=false, e=2i, m=0x0000000000000000
    n=false, e=2i, m=0x0004000000000000 -> 0x1.4p+2 -> n=false, e=2i, m=0x0004000000000000
    n=false, e=1023i, m=0x000fffffffffffff -> 0x1.fffffffffffffp+1023 -> n=false, e=1023i, m=0x000fffffffffffff
    n=false, e=-1023i, m=0x0000000000000001 -> 0x0.0000000000001p-1022 -> n=false, e=-1023i, m=0x0000000000000001
    n=false, e=-1023i, m=0x000fffffffffffff -> 0x0.fffffffffffffp-1022 -> n=false, e=-1023i, m=0x000fffffffffffff
    n=true, e=-1023i, m=0x0000000000000000 -> -0x0p+0 -> n=true, e=-1023i, m=0x0000000000000000
    n=false, e=-1023i, m=0x0000000000000000 -> 0x0p+0 -> n=false, e=-1023i, m=0x0000000000000000
    |}]

let%expect_test "m2x_f2x" =
  let open Format in
  printf "@[<h>";
  let rec fn tups = begin
    match tups with
    | [] -> ()
    | (n, e, m) :: tups' -> begin
        let f = create ~neg:n ~exponent:e ~mantissa:m in
        let m, x = m2x f in
        let f' = f2x m ~p:x in
        printf "m2x %h -> f2x %h ~p:%a -> %h\n" f m Sint.pp x f';
        fn tups'
      end
  end in
  fn [
    (* Infinite. *)
    (true, Sint.kv 1024, 0);
    (false, Sint.kv 1024, 0);

    (* Nan. *)
    (false, Sint.kv 1024, 1);
    (false, Sint.kv 1024, 0x8_0000_0000_0001);
    (false, Sint.kv 1024, 0xf_ffff_ffff_ffff);

    (* Normal. *)
    (true, Sint.kv 0, 0);
    (false, Sint.kv (-1022), 0);
    (false, Sint.kv (-52), 1);
    (false, Sint.kv (-51), 1);
    (false, Sint.kv (-1), 0);
    (false, Sint.kv 0, 0);
    (false, Sint.kv 1, 0);
    (false, Sint.kv 1, 0x8_0000_0000_0000);
    (false, Sint.kv 2, 0);
    (false, Sint.kv 2, 0x4_0000_0000_0000);
    (false, Sint.kv 1023, 0xf_ffff_ffff_ffff);

    (* Subnormal. *)
    (false, Sint.kv (-1023), 1);
    (false, Sint.kv (-1023), 0xf_ffff_ffff_ffff);

    (* Zero. *)
    (true, Sint.kv (-1023), 0);
    (false, Sint.kv (-1023), 0);
  ];
  printf "@]";

  [%expect{|
    m2x -infinity -> f2x -infinity ~p:0i -> -infinity
    m2x infinity -> f2x infinity ~p:0i -> infinity
    m2x nan -> f2x nan ~p:0i -> nan
    m2x nan -> f2x nan ~p:0i -> nan
    m2x nan -> f2x nan ~p:0i -> nan
    m2x -0x1p+0 -> f2x -0x1p-1 ~p:1i -> -0x1p+0
    m2x 0x1p-1022 -> f2x 0x1p-1 ~p:-1021i -> 0x1p-1022
    m2x 0x1.0000000000001p-52 -> f2x 0x1.0000000000001p-1 ~p:-51i -> 0x1.0000000000001p-52
    m2x 0x1.0000000000001p-51 -> f2x 0x1.0000000000001p-1 ~p:-50i -> 0x1.0000000000001p-51
    m2x 0x1p-1 -> f2x 0x1p-1 ~p:0i -> 0x1p-1
    m2x 0x1p+0 -> f2x 0x1p-1 ~p:1i -> 0x1p+0
    m2x 0x1p+1 -> f2x 0x1p-1 ~p:2i -> 0x1p+1
    m2x 0x1.8p+1 -> f2x 0x1.8p-1 ~p:2i -> 0x1.8p+1
    m2x 0x1p+2 -> f2x 0x1p-1 ~p:3i -> 0x1p+2
    m2x 0x1.4p+2 -> f2x 0x1.4p-1 ~p:3i -> 0x1.4p+2
    m2x 0x1.fffffffffffffp+1023 -> f2x 0x1.fffffffffffffp-1 ~p:1024i -> 0x1.fffffffffffffp+1023
    m2x 0x0.0000000000001p-1022 -> f2x 0x1p-1 ~p:-1073i -> 0x0.0000000000001p-1022
    m2x 0x0.fffffffffffffp-1022 -> f2x 0x1.ffffffffffffep-1 ~p:-1022i -> 0x0.fffffffffffffp-1022
    m2x -0x0p+0 -> f2x -0x0p+0 ~p:0i -> -0x0p+0
    m2x 0x0p+0 -> f2x 0x0p+0 ~p:0i -> 0x0p+0
    |}]

let%expect_test "min_max_value" =
  let open Printf in
  printf "min_value: %h\n" min_value;
  printf "max_value: %h\n" max_value;

  [%expect{|
    min_value: -infinity
    max_value: infinity
    |}]

let%expect_test "constants" =
  let open Printf in
  printf "one: %h\n" one;
  printf "neg_one: %h\n" neg_one;
  printf "nan: %h\n" nan;
  printf "inf: %h\n" inf;
  printf "neg_inf: %h\n" neg_inf;
  printf "pi: %h\n" pi;

  [%expect{|
  one: 0x1p+0
  neg_one: -0x1p+0
  nan: nan
  inf: infinity
  neg_inf: -infinity
  pi: 0x1.921fb54442d18p+1
  |}]

let%expect_test "operators" =
  let open Printf in
  let norm_nan t = if (is_nan t) then nan else t in
  for i = -1 to 2 do
    let i = Sint.of_int i in
    let t0 = of_sint i in
    for j = -1 to 2 do
      let j = Sint.of_int j in
      let t1 = of_sint j in
      printf ("+ - * / %% ** copysign %.1f ~sign:%.1f -> " ^^
          "%.1f %.1f %.1f %.1f %.1f %.1f %.1f\n")
        t0 t1 (t0 + t1) (t0 - t1) (t0 * t1) (norm_nan (t0 / t1))
        (norm_nan (t0 % t1)) (t0 ** t1) (copysign ~sign:t1 t0);
    done;
    printf "~- ~+ neg abs %.1f -> %.1f %.1f %.1f %.1f\n"
      t0 (~- t0) (~+ t0) (neg t0) (abs t0);
  done;

  [%expect{|
  + - * / % ** copysign -1.0 ~sign:-1.0 -> -2.0 0.0 1.0 1.0 -0.0 -1.0 -1.0
  + - * / % ** copysign -1.0 ~sign:0.0 -> -1.0 -1.0 -0.0 -inf nan 1.0 1.0
  + - * / % ** copysign -1.0 ~sign:1.0 -> 0.0 -2.0 -1.0 -1.0 -0.0 -1.0 1.0
  + - * / % ** copysign -1.0 ~sign:2.0 -> 1.0 -3.0 -2.0 -0.5 -1.0 1.0 1.0
  ~- ~+ neg abs -1.0 -> 1.0 -1.0 1.0 1.0
  + - * / % ** copysign 0.0 ~sign:-1.0 -> -1.0 1.0 -0.0 -0.0 0.0 inf -0.0
  + - * / % ** copysign 0.0 ~sign:0.0 -> 0.0 0.0 0.0 nan nan 1.0 0.0
  + - * / % ** copysign 0.0 ~sign:1.0 -> 1.0 -1.0 0.0 0.0 0.0 0.0 0.0
  + - * / % ** copysign 0.0 ~sign:2.0 -> 2.0 -2.0 0.0 0.0 0.0 0.0 0.0
  ~- ~+ neg abs 0.0 -> -0.0 0.0 -0.0 0.0
  + - * / % ** copysign 1.0 ~sign:-1.0 -> 0.0 2.0 -1.0 -1.0 0.0 1.0 -1.0
  + - * / % ** copysign 1.0 ~sign:0.0 -> 1.0 1.0 0.0 inf nan 1.0 1.0
  + - * / % ** copysign 1.0 ~sign:1.0 -> 2.0 0.0 1.0 1.0 0.0 1.0 1.0
  + - * / % ** copysign 1.0 ~sign:2.0 -> 3.0 -1.0 2.0 0.5 1.0 1.0 1.0
  ~- ~+ neg abs 1.0 -> -1.0 1.0 -1.0 1.0
  + - * / % ** copysign 2.0 ~sign:-1.0 -> 1.0 3.0 -2.0 -2.0 0.0 0.5 -2.0
  + - * / % ** copysign 2.0 ~sign:0.0 -> 2.0 2.0 0.0 inf nan 1.0 2.0
  + - * / % ** copysign 2.0 ~sign:1.0 -> 3.0 1.0 2.0 2.0 0.0 2.0 2.0
  + - * / % ** copysign 2.0 ~sign:2.0 -> 4.0 0.0 4.0 1.0 0.0 4.0 2.0
  ~- ~+ neg abs 2.0 -> -2.0 2.0 -2.0 2.0
  |}]

let%expect_test "classify" =
  let open Format in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "%h -> %a\n" t Class.pp (classify t);
        fn ts'
      end
  end in
  printf "@[<h>";
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    0x0.0000000000001p-1022; 0x0.fffffffffffffp-1022;
    -0.; 0.;
  ];
  printf "@]";

  [%expect{|
  infinity -> Infinite
  -infinity -> Infinite
  nan -> Nan
  -0x1p+0 -> Normal
  0x1p+0 -> Normal
  0x0.0000000000001p-1022 -> Subnormal
  0x0.fffffffffffffp-1022 -> Subnormal
  -0x0p+0 -> Zero
  0x0p+0 -> Zero
  |}]

let%expect_test "round" =
  let open Printf in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf ("round %h -> (Down: %h) (Up: %h) (Nearest: %h) ([default]: %h)"
          ^^ " (Zero: %h)\n") t (round ~dir:Down t) (round ~dir:Up t)
          (round ~dir:Nearest t) (round t) (round ~dir:Zero t);
        fn ts'
      end
  end in
  fn [
    nan;
    neg_inf;
    -0x1.0_0000_0000_0001;
    -0x1.0;
    -0x1.f_ffff_ffff_ffffp-1;
    -0x0.8_0000_0000_0001;
    -0x0.8;
    -0x0.7_ffff_ffff_ffff;
    -0x0.00_0000_0000_0008;
    -0x0.0;
    0x0.0;
    0x0.00_0000_0000_0008;
    0x0.7_ffff_ffff_ffff;
    0x0.8;
    0x0.8_0000_0000_0001;
    0x1.f_ffff_ffff_ffffp-1;
    0x1.0;
    0x1.0_0000_0000_0001;
    inf;
  ];

  [%expect{|
    round nan -> (Down: nan) (Up: nan) (Nearest: nan) ([default]: nan) (Zero: nan)
    round -infinity -> (Down: -infinity) (Up: -infinity) (Nearest: -infinity) ([default]: -infinity) (Zero: -infinity)
    round -0x1.0000000000001p+0 -> (Down: -0x1p+1) (Up: -0x1p+0) (Nearest: -0x1p+0) ([default]: -0x1p+0) (Zero: -0x1p+0)
    round -0x1p+0 -> (Down: -0x1p+0) (Up: -0x1p+0) (Nearest: -0x1p+0) ([default]: -0x1p+0) (Zero: -0x1p+0)
    round -0x1.fffffffffffffp-1 -> (Down: -0x1p+0) (Up: -0x0p+0) (Nearest: -0x1p+0) ([default]: -0x1p+0) (Zero: -0x0p+0)
    round -0x1.0000000000002p-1 -> (Down: -0x1p+0) (Up: -0x0p+0) (Nearest: -0x1p+0) ([default]: -0x1p+0) (Zero: -0x0p+0)
    round -0x1p-1 -> (Down: -0x1p+0) (Up: -0x0p+0) (Nearest: -0x1p+0) ([default]: -0x1p+0) (Zero: -0x0p+0)
    round -0x1.ffffffffffffcp-2 -> (Down: -0x1p+0) (Up: -0x0p+0) (Nearest: -0x0p+0) ([default]: -0x0p+0) (Zero: -0x0p+0)
    round -0x1p-53 -> (Down: -0x1p+0) (Up: -0x0p+0) (Nearest: -0x0p+0) ([default]: -0x0p+0) (Zero: -0x0p+0)
    round -0x0p+0 -> (Down: -0x0p+0) (Up: -0x0p+0) (Nearest: -0x0p+0) ([default]: -0x0p+0) (Zero: -0x0p+0)
    round 0x0p+0 -> (Down: 0x0p+0) (Up: 0x0p+0) (Nearest: 0x0p+0) ([default]: 0x0p+0) (Zero: 0x0p+0)
    round 0x1p-53 -> (Down: 0x0p+0) (Up: 0x1p+0) (Nearest: 0x0p+0) ([default]: 0x0p+0) (Zero: 0x0p+0)
    round 0x1.ffffffffffffcp-2 -> (Down: 0x0p+0) (Up: 0x1p+0) (Nearest: 0x0p+0) ([default]: 0x0p+0) (Zero: 0x0p+0)
    round 0x1p-1 -> (Down: 0x0p+0) (Up: 0x1p+0) (Nearest: 0x1p+0) ([default]: 0x1p+0) (Zero: 0x0p+0)
    round 0x1.0000000000002p-1 -> (Down: 0x0p+0) (Up: 0x1p+0) (Nearest: 0x1p+0) ([default]: 0x1p+0) (Zero: 0x0p+0)
    round 0x1.fffffffffffffp-1 -> (Down: 0x0p+0) (Up: 0x1p+0) (Nearest: 0x1p+0) ([default]: 0x1p+0) (Zero: 0x0p+0)
    round 0x1p+0 -> (Down: 0x1p+0) (Up: 0x1p+0) (Nearest: 0x1p+0) ([default]: 0x1p+0) (Zero: 0x1p+0)
    round 0x1.0000000000001p+0 -> (Down: 0x1p+0) (Up: 0x1p+1) (Nearest: 0x1p+0) ([default]: 0x1p+0) (Zero: 0x1p+0)
    round infinity -> (Down: infinity) (Up: infinity) (Nearest: infinity) ([default]: infinity) (Zero: infinity)
    |}]

let%expect_test "min_max" =
  let open Printf in
  for i = -1 to 1 do
    let i = Sint.of_int i in
    let t0 = of_sint i in
    for j = -1 to 1 do
      let j = Sint.of_int j in
      let t1 = of_sint j in
      printf "min max %.1f %.1f -> %.1f %.1f\n" t0 t1 (min t0 t1) (max t0 t1);
    done;
  done;

  [%expect{|
  min max -1.0 -1.0 -> -1.0 -1.0
  min max -1.0 0.0 -> -1.0 0.0
  min max -1.0 1.0 -> -1.0 1.0
  min max 0.0 -1.0 -> -1.0 0.0
  min max 0.0 0.0 -> 0.0 0.0
  min max 0.0 1.0 -> 0.0 1.0
  min max 1.0 -1.0 -> -1.0 1.0
  min max 1.0 0.0 -> 0.0 1.0
  min max 1.0 1.0 -> 1.0 1.0
  |}]

let%expect_test "ex" =
  let open Printf in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "ex %h -> %h\n" t (ex t);
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    -0.; 0.;
  ];

  [%expect{|
  ex infinity -> infinity
  ex -infinity -> 0x0p+0
  ex nan -> nan
  ex -0x1p+0 -> 0x1.78b56362cef38p-2
  ex 0x1p+0 -> 0x1.5bf0a8b145769p+1
  ex -0x0p+0 -> 0x1p+0
  ex 0x0p+0 -> 0x1p+0
  |}]

let%expect_test "lg" =
  let open Printf in
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "lg %h -> %h\n" t (norm_nan (lg t));
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    2.; 4.;
    -0.; 0.;
  ];

  [%expect{|
  lg infinity -> infinity
  lg -infinity -> nan
  lg nan -> nan
  lg -0x1p+0 -> nan
  lg 0x1p+0 -> 0x0p+0
  lg 0x1p+1 -> 0x1p+0
  lg 0x1p+2 -> 0x1p+1
  lg -0x0p+0 -> -infinity
  lg 0x0p+0 -> -infinity
  |}]

let%expect_test "ln" =
  let open Printf in
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "ln %h -> %h\n" t (norm_nan (ln t));
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    (ex 1.); (ex 2.);
    -0.; 0.;
  ];

  [%expect{|
  ln infinity -> infinity
  ln -infinity -> nan
  ln nan -> nan
  ln -0x1p+0 -> nan
  ln 0x1p+0 -> 0x0p+0
  ln 0x1.5bf0a8b145769p+1 -> 0x1p+0
  ln 0x1.d8e64b8d4ddaep+2 -> 0x1p+1
  ln -0x0p+0 -> -infinity
  ln 0x0p+0 -> -infinity
  |}]

let%expect_test "ln1p" =
  let open Printf in
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "ln,ln1p %h -> %h %h\n" t (norm_nan (ln (1. + t)))
          (norm_nan (ln1p t));
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -2.; -1.; 1.;
    (ex 1.); (ex 2.);
    0x0.0000000000001p-1022; 0x0.fffffffffffffp-1022;
    -0.; 0.;
  ];

  [%expect{|
    ln,ln1p infinity -> infinity infinity
    ln,ln1p -infinity -> nan nan
    ln,ln1p nan -> nan nan
    ln,ln1p -0x1p+1 -> nan nan
    ln,ln1p -0x1p+0 -> -infinity -infinity
    ln,ln1p 0x1p+0 -> 0x1.62e42fefa39efp-1 0x1.62e42fefa39efp-1
    ln,ln1p 0x1.5bf0a8b145769p+1 -> 0x1.5031eafefb049p+0 0x1.5031eafefb049p+0
    ln,ln1p 0x1.d8e64b8d4ddaep+2 -> 0x1.103f2d54301d5p+1 0x1.103f2d54301d5p+1
    ln,ln1p 0x0.0000000000001p-1022 -> 0x0p+0 0x0.0000000000001p-1022
    ln,ln1p 0x0.fffffffffffffp-1022 -> 0x0p+0 0x0.fffffffffffffp-1022
    ln,ln1p -0x0p+0 -> 0x0p+0 -0x0p+0
    ln,ln1p 0x0p+0 -> 0x0p+0 0x0p+0
  |}]

let%expect_test "log" =
  let open Printf in
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf "log %h -> %h\n" t (norm_nan (log t));
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    10.; 100.;
    -0.; 0.;
  ];

  [%expect{|
  log infinity -> infinity
  log -infinity -> nan
  log nan -> nan
  log -0x1p+0 -> nan
  log 0x1p+0 -> 0x0p+0
  log 0x1.4p+3 -> 0x1p+0
  log 0x1.9p+6 -> 0x1p+1
  log -0x0p+0 -> -infinity
  log 0x0p+0 -> -infinity
  |}]

let%expect_test "pow" =
  let open Format in
  printf "@[<h>";
  let rec fn pairs = begin
    match pairs with
    | [] -> ()
    | (b, x) :: pairs' -> begin
        let xf = of_sint x in
        printf "** pow int_pow %h ~p:%a -> %h %h %h\n"
          b Sint.pp x (b ** xf) (pow b ~p:xf) (int_pow b ~p:x);
        fn pairs'
      end
  end in
  fn [
    (3., Sint.kv (-3));
    (-1., Sint.kv 61);
    (1., Sint.kv 61);
    (2., Sint.kv (-1));
    (2., Sint.kv 0);
    (2., Sint.kv 1);
    (2., Sint.kv 2);
    (2., Sint.kv 61);
    (10., Sint.kv 7);
    ((ex 1.), Sint.kv (-1));
    ((ex 1.), Sint.kv 0);
    ((ex 1.), Sint.kv 1);
    ((ex 1.), Sint.kv 2);
  ];
  printf "@]";

  [%expect{|
  ** pow int_pow 0x1.8p+1 ~p:-3i -> 0x1.2f684bda12f68p-5 0x1.2f684bda12f68p-5 0x1.2f684bda12f68p-5
  ** pow int_pow -0x1p+0 ~p:61i -> -0x1p+0 -0x1p+0 -0x1p+0
  ** pow int_pow 0x1p+0 ~p:61i -> 0x1p+0 0x1p+0 0x1p+0
  ** pow int_pow 0x1p+1 ~p:-1i -> 0x1p-1 0x1p-1 0x1p-1
  ** pow int_pow 0x1p+1 ~p:0i -> 0x1p+0 0x1p+0 0x1p+0
  ** pow int_pow 0x1p+1 ~p:1i -> 0x1p+1 0x1p+1 0x1p+1
  ** pow int_pow 0x1p+1 ~p:2i -> 0x1p+2 0x1p+2 0x1p+2
  ** pow int_pow 0x1p+1 ~p:61i -> 0x1p+61 0x1p+61 0x1p+61
  ** pow int_pow 0x1.4p+3 ~p:7i -> 0x1.312dp+23 0x1.312dp+23 0x1.312dp+23
  ** pow int_pow 0x1.5bf0a8b145769p+1 ~p:-1i -> 0x1.78b56362cef38p-2 0x1.78b56362cef38p-2 0x1.78b56362cef38p-2
  ** pow int_pow 0x1.5bf0a8b145769p+1 ~p:0i -> 0x1p+0 0x1p+0 0x1p+0
  ** pow int_pow 0x1.5bf0a8b145769p+1 ~p:1i -> 0x1.5bf0a8b145769p+1 0x1.5bf0a8b145769p+1 0x1.5bf0a8b145769p+1
  ** pow int_pow 0x1.5bf0a8b145769p+1 ~p:2i -> 0x1.d8e64b8d4ddadp+2 0x1.d8e64b8d4ddadp+2 0x1.d8e64b8d4ddadp+2
  |}]

let%expect_test "lngamma" =
  let open Printf in

  for n = 1 to 40 do
    let n = Sint.of_int n in
    let x = (of_sint n) / 4. in
    printf "lngamma %.2f -> %.9f\n" x (lngamma x);
  done;

  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "lngamma %.2f -> %.5e\n" x (lngamma x);
        fn xs'
      end
  end in
  fn [neg_inf; -1.; -0.; 0.; inf; nan];

  [%expect{|
    lngamma 0.25 -> 1.288022525
    lngamma 0.50 -> 0.572364943
    lngamma 0.75 -> 0.203280951
    lngamma 1.00 -> 0.000000000
    lngamma 1.25 -> -0.098271836
    lngamma 1.50 -> -0.120782238
    lngamma 1.75 -> -0.084401121
    lngamma 2.00 -> 0.000000000
    lngamma 2.25 -> 0.124871715
    lngamma 2.50 -> 0.284682870
    lngamma 2.75 -> 0.475214667
    lngamma 3.00 -> 0.693147181
    lngamma 3.25 -> 0.935801931
    lngamma 3.50 -> 1.200973602
    lngamma 3.75 -> 1.486815579
    lngamma 4.00 -> 1.791759469
    lngamma 4.25 -> 2.114456927
    lngamma 4.50 -> 2.453736571
    lngamma 4.75 -> 2.808571419
    lngamma 5.00 -> 3.178053830
    lngamma 5.25 -> 3.561375910
    lngamma 5.50 -> 3.957813968
    lngamma 5.75 -> 4.366716037
    lngamma 6.00 -> 4.787491743
    lngamma 6.25 -> 5.219603987
    lngamma 6.50 -> 5.662562060
    lngamma 6.75 -> 6.115915891
    lngamma 7.00 -> 6.579251212
    lngamma 7.25 -> 7.052185451
    lngamma 7.50 -> 7.534364237
    lngamma 7.75 -> 8.025458396
    lngamma 8.00 -> 8.525161361
    lngamma 8.25 -> 9.033186920
    lngamma 8.50 -> 9.549267257
    lngamma 8.75 -> 10.073151240
    lngamma 9.00 -> 10.604602903
    lngamma 9.25 -> 11.143400120
    lngamma 9.50 -> 11.689333421
    lngamma 9.75 -> 12.242204940
    lngamma 10.00 -> 12.801827480
    lngamma -inf -> inf
    lngamma -1.00 -> inf
    lngamma -0.00 -> inf
    lngamma 0.00 -> inf
    lngamma inf -> inf
    lngamma nan -> nan
    |}]

let%expect_test "gamma" =
  let open Printf in
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "gamma %.2f -> %.5e\n" x (gamma x);
        fn xs'
      end
  end in
  fn [-1.; -0.; 0.; 0.5; 10.; 171.6; 171.7; inf; nan];

  [%expect{|
    gamma -1.00 -> nan
    gamma -0.00 -> -inf
    gamma 0.00 -> inf
    gamma 0.50 -> 1.77245e+00
    gamma 10.00 -> 3.62880e+05
    gamma 171.60 -> 1.58590e+308
    gamma 171.70 -> inf
    gamma inf -> inf
    gamma nan -> nan
    |}]

let%expect_test "rt" =
  let open Printf in
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "%f: sqrt=%f cbrt=%f\n" x (norm_nan (sqrt x)) (cbrt x);
        fn xs'
      end
  end in
  fn [-0.125; 0.; inf; 1.; 4.; 27.; 64.; 65.; 729.];

  [%expect{|
    -0.125000: sqrt=nan cbrt=-0.500000
    0.000000: sqrt=0.000000 cbrt=0.000000
    inf: sqrt=inf cbrt=inf
    1.000000: sqrt=1.000000 cbrt=1.000000
    4.000000: sqrt=2.000000 cbrt=1.587401
    27.000000: sqrt=5.196152 cbrt=3.000000
    64.000000: sqrt=8.000000 cbrt=4.000000
    65.000000: sqrt=8.062258 cbrt=4.020726
    729.000000: sqrt=27.000000 cbrt=9.000000
    |}]

let%expect_test "hypot" =
  let open Printf in
  let rec fn xys = begin
    match xys with
    | [] -> ()
    | (x, y) :: xys' -> begin
        printf "x=%f y=%f: hypot=%f\n" x y (hypot x y);
        fn xys'
      end
  end in
  fn [(3., 4.); (4., 3.); (-3., -4.);
    (-0., -3.); (0., -3.);
    (3., inf); (nan, inf); (neg_inf, nan);
  ];

  [%expect{|
    x=3.000000 y=4.000000: hypot=5.000000
    x=4.000000 y=3.000000: hypot=5.000000
    x=-3.000000 y=-4.000000: hypot=5.000000
    x=-0.000000 y=-3.000000: hypot=3.000000
    x=0.000000 y=-3.000000: hypot=3.000000
    x=3.000000 y=inf: hypot=inf
    x=nan y=inf: hypot=inf
    x=-inf y=nan: hypot=-inf
    |}]

let%expect_test "trig" =
  let open Printf in
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let sin_t = sin t in
        let cos_t = cos t in
        let tan_t = tan t in
        printf "sin cos tan %.5f -> %.5f %.5f %.5f\n" t sin_t cos_t tan_t;
        printf "asin acos atan atan2 -> %.5f %.5f %.5f %.5f\n"
          (asin sin_t) (acos cos_t) (atan tan_t) (atan2 sin_t cos_t);
        fn ts'
      end
  in
  fn [
    0.; (pi / 6.); (pi / 4.); (2./3. * pi); pi; (4./3. * pi); (2. * pi);
  ];

  [%expect{|
  sin cos tan 0.00000 -> 0.00000 1.00000 0.00000
  asin acos atan atan2 -> 0.00000 0.00000 0.00000 0.00000
  sin cos tan 0.52360 -> 0.50000 0.86603 0.57735
  asin acos atan atan2 -> 0.52360 0.52360 0.52360 0.52360
  sin cos tan 0.78540 -> 0.70711 0.70711 1.00000
  asin acos atan atan2 -> 0.78540 0.78540 0.78540 0.78540
  sin cos tan 2.09440 -> 0.86603 -0.50000 -1.73205
  asin acos atan atan2 -> 1.04720 2.09440 -1.04720 2.09440
  sin cos tan 3.14159 -> 0.00000 -1.00000 -0.00000
  asin acos atan atan2 -> 0.00000 3.14159 -0.00000 3.14159
  sin cos tan 4.18879 -> -0.86603 -0.50000 1.73205
  asin acos atan atan2 -> -1.04720 2.09440 1.04720 -2.09440
  sin cos tan 6.28319 -> -0.00000 1.00000 -0.00000
  asin acos atan atan2 -> -0.00000 0.00000 -0.00000 -0.00000
  |}]

let%expect_test "trigh" =
  let open Printf in
  for i = -2 to 2 do
    let i = Sint.of_int i in
    let t = of_sint i in
    printf ("sinh cosh tanh %.1f -> (%.5f %.5f) (%.5f %.5f) (%.5f %.5f)\n")
      t
      (sinh t) (((ex t) - (ex ~-t)) / 2.)
      (cosh t) (((ex t) + (ex ~-t)) / 2.)
      (tanh t) ((sinh t) / (cosh t));
  done;

  [%expect{|
  sinh cosh tanh -2.0 -> (-3.62686 -3.62686) (3.76220 3.76220) (-0.96403 -0.96403)
  sinh cosh tanh -1.0 -> (-1.17520 -1.17520) (1.54308 1.54308) (-0.76159 -0.76159)
  sinh cosh tanh 0.0 -> (0.00000 0.00000) (1.00000 1.00000) (0.00000 0.00000)
  sinh cosh tanh 1.0 -> (1.17520 1.17520) (1.54308 1.54308) (0.76159 0.76159)
  sinh cosh tanh 2.0 -> (3.62686 3.62686) (3.76220 3.76220) (0.96403 0.96403)
  |}]
