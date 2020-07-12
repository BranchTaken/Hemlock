(* Partial Rudiments. *)
module Sint = I63
module Uns = U63
open Rudiments_int
open Rudiments_functions

module T = struct
  type t = sint
  let num_bits = 32
end
include T
include Intnb.Make_i(T)

let to_sint t =
  t

let of_sint x =
  narrow_of_signed x

let of_sint_hlt x =
  let t = of_sint x in
  let x' = to_sint t in
  match Sint.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let kv x =
  narrow_of_signed (sint_of_int x)

let to_uns t =
  uns_of_sint t

let of_uns x =
  narrow_of_unsigned x

let of_uns_hlt x =
  let t = of_uns x in
  let x' = to_uns t in
  match Uns.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x x Hash.pp (Hash.t_of_state (hash_fold x Hash.State.empty));
        test_hash_fold xs'
      end
  end in
  let xs = [min_value; neg_one; zero; one; max_value] in
  test_hash_fold xs;
  printf "@]";

  [%expect{|
    hash_fold 0x80000000i32 -> 0xe86f_5881_c5da_3e37_d051_e811_07ee_cedfu128
    hash_fold 0xffffffffi32 -> 0x913b_a441_dcb5_f088_efa8_6f78_1580_b321u128
    hash_fold 0x00000000i32 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x00000001i32 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x7fffffffi32 -> 0x65c8_a621_a40c_1c69_b29f_e175_1446_97a2u128
    |}]

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
  fn [min_value; neg_one; zero; one; max_value];
  printf "@]";

  [%expect{|
    -2147483648i32 0x80000000i32
    -1i32 0xffffffffi32
    0i32 0x00000000i32
    1i32 0x00000001i32
    2147483647i32 0x7fffffffi32
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Uns.pp num_bits;
  printf "min_value=%a %a\n" pp min_value pp_x min_value;
  printf "max_value=%a %a\n" pp max_value pp_x max_value;

  [%expect{|
    num_bits=32
    min_value=-2147483648i32 0x80000000i32
    max_value=2147483647i32 0x7fffffffi32
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
  fn (kv 0) (kv (-0x8000_0000));
  printf "\n";
  fn (kv 0) (kv (-1));
  printf "\n";
  fn (kv (-0x8000_0000)) (kv (-1));
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv (-2)) (kv (-1)) (kv 0);
  fn2 (kv (-1)) (kv (-1)) (kv 0);
  fn2 (kv 0) (kv (-1)) (kv 0);
  fn2 (kv 1) (kv (-1)) (kv 0);

  [%expect{|
    cmp 0x00000000i32 0x80000000i32 -> Gt
    0x00000000i32 >= 0x80000000i32 -> true
    0x00000000i32 <= 0x80000000i32 -> false
    0x00000000i32 = 0x80000000i32 -> false
    0x00000000i32 > 0x80000000i32 -> true
    0x00000000i32 < 0x80000000i32 -> false
    0x00000000i32 <> 0x80000000i32 -> true
    ascending 0x00000000i32 0x80000000i32 -> Gt
    descending 0x00000000i32 0x80000000i32 -> Lt

    cmp 0x00000000i32 0xffffffffi32 -> Gt
    0x00000000i32 >= 0xffffffffi32 -> true
    0x00000000i32 <= 0xffffffffi32 -> false
    0x00000000i32 = 0xffffffffi32 -> false
    0x00000000i32 > 0xffffffffi32 -> true
    0x00000000i32 < 0xffffffffi32 -> false
    0x00000000i32 <> 0xffffffffi32 -> true
    ascending 0x00000000i32 0xffffffffi32 -> Gt
    descending 0x00000000i32 0xffffffffi32 -> Lt

    cmp 0x80000000i32 0xffffffffi32 -> Lt
    0x80000000i32 >= 0xffffffffi32 -> false
    0x80000000i32 <= 0xffffffffi32 -> true
    0x80000000i32 = 0xffffffffi32 -> false
    0x80000000i32 > 0xffffffffi32 -> false
    0x80000000i32 < 0xffffffffi32 -> true
    0x80000000i32 <> 0xffffffffi32 -> true
    ascending 0x80000000i32 0xffffffffi32 -> Lt
    descending 0x80000000i32 0xffffffffi32 -> Gt

    clamp ~min:0xffffffffi32 ~max:0x00000000i32 0xfffffffei32 -> 0xffffffffi32
    between ~low:0xffffffffi32 ~high:0x00000000i32 0xfffffffei32 -> false

    clamp ~min:0xffffffffi32 ~max:0x00000000i32 0xffffffffi32 -> 0xffffffffi32
    between ~low:0xffffffffi32 ~high:0x00000000i32 0xffffffffi32 -> true

    clamp ~min:0xffffffffi32 ~max:0x00000000i32 0x00000000i32 -> 0x00000000i32
    between ~low:0xffffffffi32 ~high:0x00000000i32 0x00000000i32 -> true

    clamp ~min:0xffffffffi32 ~max:0x00000000i32 0x00000001i32 -> 0x00000000i32
    between ~low:0xffffffffi32 ~high:0x00000000i32 0x00000001i32 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);

  [%expect{|
    max_value + 1i32 -> 0x80000000i32
    min_value - 1i32 -> 0x7fffffffi32
    |}]

let%expect_test "conversion" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = sint_of_int x in
        let t = of_sint i in
        let i' = to_sint t in
        let t' = of_sint i' in
        printf "of_sint %a -> to_sint %a -> of_sint %a -> %a\n"
          Sint.pp_x i pp_x t Sint.pp i' pp_x t';
        let t = of_uns (Uns.of_sint i) in
        let u = to_uns t in
        let t' = of_uns u in
        printf "of_uns %a -> to_uns %a -> of_uns %a -> %a\n"
          Sint.pp_x i pp_x t Uns.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Uns.max_value; (-2); (-1); 0; 1; 2; Uns.of_sint Sint.max_value];

  [%expect{|
    of_sint 0x7fffffffffffffffi -> to_sint 0xffffffffi32 -> of_sint -1i -> 0xffffffffi32
    of_uns 0x7fffffffffffffffi -> to_uns 0x7fffffffi32 -> of_uns 0x000000007fffffff -> 0x7fffffffi32
    of_sint 0x7ffffffffffffffei -> to_sint 0xfffffffei32 -> of_sint -2i -> 0xfffffffei32
    of_uns 0x7ffffffffffffffei -> to_uns 0x7ffffffei32 -> of_uns 0x000000007ffffffe -> 0x7ffffffei32
    of_sint 0x7fffffffffffffffi -> to_sint 0xffffffffi32 -> of_sint -1i -> 0xffffffffi32
    of_uns 0x7fffffffffffffffi -> to_uns 0x7fffffffi32 -> of_uns 0x000000007fffffff -> 0x7fffffffi32
    of_sint 0x0000000000000000i -> to_sint 0x00000000i32 -> of_sint 0i -> 0x00000000i32
    of_uns 0x0000000000000000i -> to_uns 0x00000000i32 -> of_uns 0x0000000000000000 -> 0x00000000i32
    of_sint 0x0000000000000001i -> to_sint 0x00000001i32 -> of_sint 1i -> 0x00000001i32
    of_uns 0x0000000000000001i -> to_uns 0x00000001i32 -> of_uns 0x0000000000000001 -> 0x00000001i32
    of_sint 0x0000000000000002i -> to_sint 0x00000002i32 -> of_sint 2i -> 0x00000002i32
    of_uns 0x0000000000000002i -> to_uns 0x00000002i32 -> of_uns 0x0000000000000002 -> 0x00000002i32
    of_sint 0x3fffffffffffffffi -> to_sint 0x7fffffffi32 -> of_sint 2147483647i -> 0x7fffffffi32
    of_uns 0x3fffffffffffffffi -> to_uns 0x7fffffffi32 -> of_uns 0x000000007fffffff -> 0x7fffffffi32
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

    0x1.f_ffff_ffff_ffffp61;

    0x1p30;
    0x1p31;
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
    neg_one;
    zero;
    one;
    max_value;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_float -0x1p+0 -> 0xffffffffi32; to_float -> -0x1p+0
    of_float 0x0p+0 -> 0x00000000i32; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x00000000i32; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x00000001i32; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0x7fffffffi32; to_float -> 0x1.fffffffcp+30
    of_float 0x1.fffffffffffffp+52 -> 0x7fffffffi32; to_float -> 0x1.fffffffcp+30
    of_float 0x1.fffffffffffffp+56 -> 0x7ffffff0i32; to_float -> 0x1.ffffffcp+30
    of_float 0x1.fffffffffffffp+61 -> 0x7ffffe00i32; to_float -> 0x1.fffff8p+30
    of_float 0x1p+30 -> 0x40000000i32; to_float -> 0x1p+30
    of_float 0x1p+31 -> 0x00000000i32; to_float -> 0x0p+0

    to_float 0xffffffffi32 -> -0x1p+0; of_float -> 0xffffffffi32
    to_float 0x00000000i32 -> 0x0p+0; of_float -> 0x00000000i32
    to_float 0x00000001i32 -> 0x1p+0; of_float -> 0x00000001i32
    to_float 0x7fffffffi32 -> 0x1.fffffffcp+30; of_float -> 0x7fffffffi32
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
    (kv 0, kv 0);
    (kv 0xffff_ffff, kv 0);
    (kv 0, kv 0xffff_ffff);
    (kv 0xffff_ffff, kv 0xffff_ffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x00000000i32 0x00000000i32 -> 0x00000000i32, 0x00000000i32, 0x00000000i32
    bit_{and,or,xor} 0x7fffffffi32 0x00000000i32 -> 0x00000000i32, 0x7fffffffi32, 0x7fffffffi32
    bit_{and,or,xor} 0x00000000i32 0x7fffffffi32 -> 0x00000000i32, 0x7fffffffi32, 0x7fffffffi32
    bit_{and,or,xor} 0x7fffffffi32 0x7fffffffi32 -> 0x7fffffffi32, 0x7fffffffi32, 0x00000000i32
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
    kv 0;
    kv 0xffff_ffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x00000000i32 -> 0xffffffffi32
    bit_not 0x7fffffffi32 -> 0x80000000i32
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
    kv (-0x8000_0000);
    kv (-1);
    kv 0;
    kv 1;
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x80000000i32 -> 1, 0, 31
    bit_{pop,clz,ctz} 0xffffffffi32 -> 32, 0, 0
    bit_{pop,clz,ctz} 0x00000000i32 -> 0, 32, 32
    bit_{pop,clz,ctz} 0x00000001i32 -> 1, 31, 0
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
    (kv 0, kv 0);
    (kv 0, kv 1);
    (kv 1, kv 0);
    (kv 1, kv 1);

    (kv (-1), kv 0);
    (kv (-1), kv 1);

    (kv 2, kv 15);
    (kv 2, kv 16);
    (kv 2, kv 31);
    (kv 2, kv 32);

    (kv 0xf, kv 0xf);
    (kv 0xff, kv 0xff);

    (kv 1, kv (-1));

    (kv (-1), kv (-1));

  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x00000000i32 ** 0x00000000i32 -> 0x00000001i32
    0x00000000i32 ** 0x00000001i32 -> 0x00000000i32
    0x00000001i32 ** 0x00000000i32 -> 0x00000001i32
    0x00000001i32 ** 0x00000001i32 -> 0x00000001i32
    0xffffffffi32 ** 0x00000000i32 -> 0x00000001i32
    0xffffffffi32 ** 0x00000001i32 -> 0xffffffffi32
    0x00000002i32 ** 0x0000000fi32 -> 0x00008000i32
    0x00000002i32 ** 0x00000010i32 -> 0x00010000i32
    0x00000002i32 ** 0x0000001fi32 -> 0x80000000i32
    0x00000002i32 ** 0x00000020i32 -> 0x00000000i32
    0x0000000fi32 ** 0x0000000fi32 -> 0x597707efi32
    0x000000ffi32 ** 0x000000ffi32 -> 0x007ffeffi32
    0x00000001i32 ** 0xffffffffi32 -> 0x00000001i32
    0xffffffffi32 ** 0xffffffffi32 -> 0xffffffffi32
    |}]
