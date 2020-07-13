(* Partial Rudiments. *)
module Uns = U63
module Sint = I63
open Rudiments_int
open Rudiments_functions

module T = struct
  type t = uns
  let num_bits = 21
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

let of_char c =
  Stdlib.Char.code c

let nul = 0x00
let soh = 0x01
let stx = 0x02
let etx = 0x03
let eot = 0x04
let enq = 0x05
let ack = 0x06
let bel = 0x07
let bs = 0x08
let ht = of_char '\t'
let lf = of_char '\n'
let nl = of_char '\n'
let vt = 0x0b
let ff = 0x0c
let cr = of_char '\r'
let so = 0x0e
let si = 0x0f
let dle = 0x10
let dc1 = 0x11
let dc2 = 0x12
let dc3 = 0x13
let dc4 = 0x14
let nak = 0x15
let syn = 0x16
let etb = 0x17
let can = 0x18
let em = 0x19
let sub = 0x1a
let esc = 0x1b
let fs = 0x1c
let gs = 0x1d
let rs = 0x1e
let us = 0x1f
let del = 0x7f

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
    hash_fold 0x000000u21 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x000001u21 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x000000u21 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x1fffffu21 -> 0xef81_2007_0e4f_2161_ed46_d40a_ca8b_7e28u128
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
  fn [kv 0; kv 1; kv 42; kv 0x7fffff];
  printf "@]";

  [%expect{|
    0u21 0x000000u21
    1u21 0x000001u21
    42u21 0x00002au21
    2097151u21 0x1fffffu21
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Uns.pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=21
    min_value=0x000000u21
    max_value=0x1fffffu21
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
  fn (kv 0) (kv 0x10_0000);
  printf "\n";
  fn (kv 0) (kv 0x1f_ffff);
  printf "\n";
  fn (kv 0x10_0000) (kv 0x1f_ffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv 0x0f_fffe) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x0f_ffff) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0000) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0001) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0002) (kv 0x0f_ffff) (kv 0x10_0001);

  [%expect{|
    cmp 0x000000u21 0x100000u21 -> Lt
    0x000000u21 >= 0x100000u21 -> false
    0x000000u21 <= 0x100000u21 -> true
    0x000000u21 = 0x100000u21 -> false
    0x000000u21 > 0x100000u21 -> false
    0x000000u21 < 0x100000u21 -> true
    0x000000u21 <> 0x100000u21 -> true
    ascending 0x000000u21 0x100000u21 -> Lt
    descending 0x000000u21 0x100000u21 -> Gt

    cmp 0x000000u21 0x1fffffu21 -> Lt
    0x000000u21 >= 0x1fffffu21 -> false
    0x000000u21 <= 0x1fffffu21 -> true
    0x000000u21 = 0x1fffffu21 -> false
    0x000000u21 > 0x1fffffu21 -> false
    0x000000u21 < 0x1fffffu21 -> true
    0x000000u21 <> 0x1fffffu21 -> true
    ascending 0x000000u21 0x1fffffu21 -> Lt
    descending 0x000000u21 0x1fffffu21 -> Gt

    cmp 0x100000u21 0x1fffffu21 -> Lt
    0x100000u21 >= 0x1fffffu21 -> false
    0x100000u21 <= 0x1fffffu21 -> true
    0x100000u21 = 0x1fffffu21 -> false
    0x100000u21 > 0x1fffffu21 -> false
    0x100000u21 < 0x1fffffu21 -> true
    0x100000u21 <> 0x1fffffu21 -> true
    ascending 0x100000u21 0x1fffffu21 -> Lt
    descending 0x100000u21 0x1fffffu21 -> Gt

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x0ffffeu21 -> 0x0fffffu21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x0ffffeu21 -> false

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x0fffffu21 -> 0x0fffffu21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x0fffffu21 -> true

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x100000u21 -> 0x100000u21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x100000u21 -> true

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x100001u21 -> 0x100001u21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x100001u21 -> true

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x100002u21 -> 0x100001u21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x100002u21 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  let fifteen = (kv 15) in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1u21 -> 0x000000u21
    min_value - 1u21 -> 0x1fffffu21
    max_value * 15u21 -> 0x1ffff1u21
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
    of_sint 0x7fffffffffffffffi -> to_sint 0x1fffffu21 -> of_sint 0x00000000001fffffi -> 0x1fffffu21
    of_uns 0x7fffffffffffffff -> to_uns 0x1fffffu21 -> of_uns 0x00000000001fffff -> 0x1fffffu21
    of_sint 0x0000000000000000i -> to_sint 0x000000u21 -> of_sint 0x0000000000000000i -> 0x000000u21
    of_uns 0x0000000000000000 -> to_uns 0x000000u21 -> of_uns 0x0000000000000000 -> 0x000000u21
    of_sint 0x000000000000002ai -> to_sint 0x00002au21 -> of_sint 0x000000000000002ai -> 0x00002au21
    of_uns 0x000000000000002a -> to_uns 0x00002au21 -> of_uns 0x000000000000002a -> 0x00002au21
    of_sint 0x00000000001fffffi -> to_sint 0x1fffffu21 -> of_sint 0x00000000001fffffi -> 0x1fffffu21
    of_uns 0x00000000001fffff -> to_uns 0x1fffffu21 -> of_uns 0x00000000001fffff -> 0x1fffffu21
    of_sint 0x0000000000200000i -> to_sint 0x000000u21 -> of_sint 0x0000000000000000i -> 0x000000u21
    of_uns 0x0000000000200000 -> to_uns 0x000000u21 -> of_uns 0x0000000000000000 -> 0x000000u21
    of_sint 0x0000000000200001i -> to_sint 0x000001u21 -> of_sint 0x0000000000000001i -> 0x000001u21
    of_uns 0x0000000000200001 -> to_uns 0x000001u21 -> of_uns 0x0000000000000001 -> 0x000001u21
    of_sint 0x3fffffffffffffffi -> to_sint 0x1fffffu21 -> of_sint 0x00000000001fffffi -> 0x1fffffu21
    of_uns 0x3fffffffffffffff -> to_uns 0x1fffffu21 -> of_uns 0x00000000001fffff -> 0x1fffffu21
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

    0x1p20;
    0x1p21;
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
    of_float -0x1p+0 -> 0x000000u21; to_float -> 0x0p+0
    of_float 0x0p+0 -> 0x000000u21; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x000000u21; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x000001u21; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0x1fffffu21; to_float -> 0x1.fffffp+20
    of_float 0x1.fffffffffffffp+52 -> 0x1fffffu21; to_float -> 0x1.fffffp+20
    of_float 0x1.fffffffffffffp+56 -> 0x1ffff0u21; to_float -> 0x1.ffffp+20
    of_float 0x1.fffffffffffffp+61 -> 0x1ffe00u21; to_float -> 0x1.ffep+20
    of_float 0x1p+20 -> 0x100000u21; to_float -> 0x1p+20
    of_float 0x1p+21 -> 0x000000u21; to_float -> 0x0p+0

    to_float 0x000000u21 -> 0x0p+0; of_float -> 0x000000u21
    to_float 0x000001u21 -> 0x1p+0; of_float -> 0x000001u21
    to_float 0x1fffffu21 -> 0x1.fffffp+20; of_float -> 0x1fffffu21
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
    (kv 0x1f_ffff, kv 0);
    (kv 0, kv 0x1f_ffff);
    (kv 0x1f_ffff, kv 0x1f_ffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x000000u21 0x000000u21 -> 0x000000u21, 0x000000u21, 0x000000u21
    bit_{and,or,xor} 0x1fffffu21 0x000000u21 -> 0x000000u21, 0x1fffffu21, 0x1fffffu21
    bit_{and,or,xor} 0x000000u21 0x1fffffu21 -> 0x000000u21, 0x1fffffu21, 0x1fffffu21
    bit_{and,or,xor} 0x1fffffu21 0x1fffffu21 -> 0x1fffffu21, 0x1fffffu21, 0x000000u21
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
    kv 0x1f_ffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x000000u21 -> 0x1fffffu21
    bit_not 0x1fffffu21 -> 0x000000u21
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
    kv 0x10_0000;
    kv 0x1f_ffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x000000u21 -> 0, 21, 21
    bit_{pop,clz,ctz} 0x000001u21 -> 1, 20, 0
    bit_{pop,clz,ctz} 0x100000u21 -> 1, 0, 20
    bit_{pop,clz,ctz} 0x1fffffu21 -> 21, 0, 0
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

    (0x1f_ffff, 0);
    (0x1f_ffff, 1);

    (2, 15);
    (2, 16);
    (2, 20);
    (2, 21);

    (0xf, 0xf);
    (0xff, 0xff);

    (1, 0x1f_ffff);

    (0x1f_ffff, 0x1f_ffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x000000u21 ** 0x000000u21 -> 0x000001u21
    0x000000u21 ** 0x000001u21 -> 0x000000u21
    0x1fffffu21 ** 0x000000u21 -> 0x000001u21
    0x1fffffu21 ** 0x000001u21 -> 0x1fffffu21
    0x000002u21 ** 0x00000fu21 -> 0x008000u21
    0x000002u21 ** 0x000010u21 -> 0x010000u21
    0x000002u21 ** 0x000014u21 -> 0x100000u21
    0x000002u21 ** 0x000015u21 -> 0x000000u21
    0x00000fu21 ** 0x00000fu21 -> 0x1707efu21
    0x0000ffu21 ** 0x0000ffu21 -> 0x1ffeffu21
    0x000001u21 ** 0x1fffffu21 -> 0x000001u21
    0x1fffffu21 ** 0x1fffffu21 -> 0x1fffffu21
    |}]
