(* Partial Rudiments. *)
module Uns = U63
open Rudiments_int
open Rudiments_functions

module T = struct
  type t = uns
  let num_bits = 16
end
include T
include Intnb.Make_u(T)

let to_sint t =
  Uns.to_sint t

let of_sint x =
  narrow_of_signed x

let of_sint_hlt x =
  let t = of_sint x in
  let x' = to_sint t in
  match Sint.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let kv x =
  narrow_of_unsigned x

let to_uns t =
  t

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
    hash_fold 0x0000u16 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x0001u16 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x0000u16 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0xffffu16 -> 0xf906_730b_3136_7fd4_9f0a_ae5d_b01d_d270u128
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
  fn [kv 0; kv 1; kv 42; kv 0x1fff];
  printf "@]";

  [%expect{|
    0u16 0x0000u16
    1u16 0x0001u16
    42u16 0x002au16
    8191u16 0x1fffu16
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Uns.pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=16
    min_value=0x0000u16
    max_value=0xffffu16
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
  fn (kv 0) (kv 0x8000);
  printf "\n";
  fn (kv 0) (kv 0xffff);
  printf "\n";
  fn (kv 0x8000) (kv 0xffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv 0x7ffe) (kv 0x7fff) (kv 0x8001);
  fn2 (kv 0x7fff) (kv 0x7fff) (kv 0x8001);
  fn2 (kv 0x8000) (kv 0x7fff) (kv 0x8001);
  fn2 (kv 0x8001) (kv 0x7fff) (kv 0x8001);
  fn2 (kv 0x8002) (kv 0x7fff) (kv 0x8001);

  [%expect{|
    cmp 0x0000u16 0x8000u16 -> Lt
    0x0000u16 >= 0x8000u16 -> false
    0x0000u16 <= 0x8000u16 -> true
    0x0000u16 = 0x8000u16 -> false
    0x0000u16 > 0x8000u16 -> false
    0x0000u16 < 0x8000u16 -> true
    0x0000u16 <> 0x8000u16 -> true
    ascending 0x0000u16 0x8000u16 -> Lt
    descending 0x0000u16 0x8000u16 -> Gt

    cmp 0x0000u16 0xffffu16 -> Lt
    0x0000u16 >= 0xffffu16 -> false
    0x0000u16 <= 0xffffu16 -> true
    0x0000u16 = 0xffffu16 -> false
    0x0000u16 > 0xffffu16 -> false
    0x0000u16 < 0xffffu16 -> true
    0x0000u16 <> 0xffffu16 -> true
    ascending 0x0000u16 0xffffu16 -> Lt
    descending 0x0000u16 0xffffu16 -> Gt

    cmp 0x8000u16 0xffffu16 -> Lt
    0x8000u16 >= 0xffffu16 -> false
    0x8000u16 <= 0xffffu16 -> true
    0x8000u16 = 0xffffu16 -> false
    0x8000u16 > 0xffffu16 -> false
    0x8000u16 < 0xffffu16 -> true
    0x8000u16 <> 0xffffu16 -> true
    ascending 0x8000u16 0xffffu16 -> Lt
    descending 0x8000u16 0xffffu16 -> Gt

    clamp ~min:0x7fffu16 ~max:0x8001u16 0x7ffeu16 -> 0x7fffu16
    between ~low:0x7fffu16 ~high:0x8001u16 0x7ffeu16 -> false

    clamp ~min:0x7fffu16 ~max:0x8001u16 0x7fffu16 -> 0x7fffu16
    between ~low:0x7fffu16 ~high:0x8001u16 0x7fffu16 -> true

    clamp ~min:0x7fffu16 ~max:0x8001u16 0x8000u16 -> 0x8000u16
    between ~low:0x7fffu16 ~high:0x8001u16 0x8000u16 -> true

    clamp ~min:0x7fffu16 ~max:0x8001u16 0x8001u16 -> 0x8001u16
    between ~low:0x7fffu16 ~high:0x8001u16 0x8001u16 -> true

    clamp ~min:0x7fffu16 ~max:0x8001u16 0x8002u16 -> 0x8001u16
    between ~low:0x7fffu16 ~high:0x8001u16 0x8002u16 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  let fifteen = (kv 15) in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1u16 -> 0x0000u16
    min_value - 1u16 -> 0xffffu16
    max_value * 15u16 -> 0xfff1u16
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
          Sint.pp_x i pp_x t Sint.pp_x i' pp_x t';
        let t = of_uns (Uns.of_sint i) in
        let u = to_uns t in
        let t' = of_uns u in
        printf "of_uns %a -> to_uns %a -> of_uns %a -> %a\n"
          Uns.pp_x x pp_x t Uns.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Uns.max_value; 0; 42; 0x1f_ffff; 0x20_0000; 0x20_0001;
    Uns.of_sint Sint.max_value];

  [%expect{|
    of_sint 0x7fffffffffffffffi -> to_sint 0xffffu16 -> of_sint 0x000000000000ffffi -> 0xffffu16
    of_uns 0x7fffffffffffffff -> to_uns 0xffffu16 -> of_uns 0x000000000000ffff -> 0xffffu16
    of_sint 0x0000000000000000i -> to_sint 0x0000u16 -> of_sint 0x0000000000000000i -> 0x0000u16
    of_uns 0x0000000000000000 -> to_uns 0x0000u16 -> of_uns 0x0000000000000000 -> 0x0000u16
    of_sint 0x000000000000002ai -> to_sint 0x002au16 -> of_sint 0x000000000000002ai -> 0x002au16
    of_uns 0x000000000000002a -> to_uns 0x002au16 -> of_uns 0x000000000000002a -> 0x002au16
    of_sint 0x00000000001fffffi -> to_sint 0xffffu16 -> of_sint 0x000000000000ffffi -> 0xffffu16
    of_uns 0x00000000001fffff -> to_uns 0xffffu16 -> of_uns 0x000000000000ffff -> 0xffffu16
    of_sint 0x0000000000200000i -> to_sint 0x0000u16 -> of_sint 0x0000000000000000i -> 0x0000u16
    of_uns 0x0000000000200000 -> to_uns 0x0000u16 -> of_uns 0x0000000000000000 -> 0x0000u16
    of_sint 0x0000000000200001i -> to_sint 0x0001u16 -> of_sint 0x0000000000000001i -> 0x0001u16
    of_uns 0x0000000000200001 -> to_uns 0x0001u16 -> of_uns 0x0000000000000001 -> 0x0001u16
    of_sint 0x3fffffffffffffffi -> to_sint 0xffffu16 -> of_sint 0x000000000000ffffi -> 0xffffu16
    of_uns 0x3fffffffffffffff -> to_uns 0xffffu16 -> of_uns 0x000000000000ffff -> 0xffffu16
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

    0x1p31;
    0x1p32;
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
    of_float -0x1p+0 -> 0x0000u16; to_float -> 0x0p+0
    of_float 0x0p+0 -> 0x0000u16; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x0000u16; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x0001u16; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0xffffu16; to_float -> 0x1.fffep+15
    of_float 0x1.fffffffffffffp+52 -> 0xffffu16; to_float -> 0x1.fffep+15
    of_float 0x1.fffffffffffffp+56 -> 0xfff0u16; to_float -> 0x1.ffep+15
    of_float 0x1.fffffffffffffp+61 -> 0xfe00u16; to_float -> 0x1.fcp+15
    of_float 0x1p+31 -> 0x0000u16; to_float -> 0x0p+0
    of_float 0x1p+32 -> 0x0000u16; to_float -> 0x0p+0

    to_float 0x0000u16 -> 0x0p+0; of_float -> 0x0000u16
    to_float 0x0001u16 -> 0x1p+0; of_float -> 0x0001u16
    to_float 0xffffu16 -> 0x1.fffep+15; of_float -> 0xffffu16
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
    (kv 0xffff, kv 0);
    (kv 0, kv 0xffff);
    (kv 0xffff, kv 0xffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0000u16 0x0000u16 -> 0x0000u16, 0x0000u16, 0x0000u16
    bit_{and,or,xor} 0xffffu16 0x0000u16 -> 0x0000u16, 0xffffu16, 0xffffu16
    bit_{and,or,xor} 0x0000u16 0xffffu16 -> 0x0000u16, 0xffffu16, 0xffffu16
    bit_{and,or,xor} 0xffffu16 0xffffu16 -> 0xffffu16, 0xffffu16, 0x0000u16
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
    kv 0xffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0000u16 -> 0xffffu16
    bit_not 0xffffu16 -> 0x0000u16
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
    kv 0;
    kv 1;
    kv 0x8000;
    kv 0xffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x0000u16 -> 0, 16, 16
    bit_{pop,clz,ctz} 0x0001u16 -> 1, 15, 0
    bit_{pop,clz,ctz} 0x8000u16 -> 1, 0, 15
    bit_{pop,clz,ctz} 0xffffu16 -> 16, 0, 0
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
    (0, 0);
    (0, 1);

    (0xffff, 0);
    (0xffff, 1);

    (2, 15);
    (2, 16);
    (2, 31);
    (2, 32);

    (0xf, 0xf);
    (0xff, 0xff);

    (1, 0xffff);

    (0xffff, 0xffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000u16 ** 0x0000u16 -> 0x0001u16
    0x0000u16 ** 0x0001u16 -> 0x0000u16
    0xffffu16 ** 0x0000u16 -> 0x0001u16
    0xffffu16 ** 0x0001u16 -> 0xffffu16
    0x0002u16 ** 0x000fu16 -> 0x8000u16
    0x0002u16 ** 0x0010u16 -> 0x0000u16
    0x0002u16 ** 0x001fu16 -> 0x0000u16
    0x0002u16 ** 0x0020u16 -> 0x0000u16
    0x000fu16 ** 0x000fu16 -> 0x07efu16
    0x00ffu16 ** 0x00ffu16 -> 0xfeffu16
    0x0001u16 ** 0xffffu16 -> 0x0001u16
    0xffffu16 ** 0xffffu16 -> 0xffffu16
    |}]
