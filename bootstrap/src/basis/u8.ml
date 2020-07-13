(* Partial Rudiments. *)
module Sint = I63
module Uns = U63
type codepoint = Codepoint.t
open Rudiments_int
open Rudiments_functions

module T = struct
  type t = uns
  let num_bits = 8
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

let to_codepoint t =
  Codepoint.of_uns (to_uns t)

let of_codepoint x =
  narrow_of_unsigned (Codepoint.to_uns x)

let of_codepoint_hlt x =
  let t = of_codepoint x in
  let x' = to_codepoint (to_uns t) in
  match Codepoint.(x' = x) with
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
    hash_fold 0x00u8 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x01u8 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x00u8 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0xffu8 -> 0xa6f2_bc68_c412_6de2_d5a3_2eb4_2449_e64du128
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
  fn [kv 0; kv 1; kv 42; kv 255];
  printf "@]";

  [%expect{|
0u8 0x00u8
1u8 0x01u8
42u8 0x2au8
255u8 0xffu8
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Uns.pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=8
    min_value=0x00u8
    max_value=0xffu8
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
  fn (kv 0) (kv 0x80);
  printf "\n";
  fn (kv 0) (kv 0xff);
  printf "\n";
  fn (kv 0x80) (kv 0xff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv 0x7e) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x7f) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x80) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x81) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x82) (kv 0x7f) (kv 0x81);

  [%expect{|
    cmp 0x00u8 0x80u8 -> Lt
    0x00u8 >= 0x80u8 -> false
    0x00u8 <= 0x80u8 -> true
    0x00u8 = 0x80u8 -> false
    0x00u8 > 0x80u8 -> false
    0x00u8 < 0x80u8 -> true
    0x00u8 <> 0x80u8 -> true
    ascending 0x00u8 0x80u8 -> Lt
    descending 0x00u8 0x80u8 -> Gt

    cmp 0x00u8 0xffu8 -> Lt
    0x00u8 >= 0xffu8 -> false
    0x00u8 <= 0xffu8 -> true
    0x00u8 = 0xffu8 -> false
    0x00u8 > 0xffu8 -> false
    0x00u8 < 0xffu8 -> true
    0x00u8 <> 0xffu8 -> true
    ascending 0x00u8 0xffu8 -> Lt
    descending 0x00u8 0xffu8 -> Gt

    cmp 0x80u8 0xffu8 -> Lt
    0x80u8 >= 0xffu8 -> false
    0x80u8 <= 0xffu8 -> true
    0x80u8 = 0xffu8 -> false
    0x80u8 > 0xffu8 -> false
    0x80u8 < 0xffu8 -> true
    0x80u8 <> 0xffu8 -> true
    ascending 0x80u8 0xffu8 -> Lt
    descending 0x80u8 0xffu8 -> Gt

    clamp ~min:0x7fu8 ~max:0x81u8 0x7eu8 -> 0x7fu8
    between ~low:0x7fu8 ~high:0x81u8 0x7eu8 -> false

    clamp ~min:0x7fu8 ~max:0x81u8 0x7fu8 -> 0x7fu8
    between ~low:0x7fu8 ~high:0x81u8 0x7fu8 -> true

    clamp ~min:0x7fu8 ~max:0x81u8 0x80u8 -> 0x80u8
    between ~low:0x7fu8 ~high:0x81u8 0x80u8 -> true

    clamp ~min:0x7fu8 ~max:0x81u8 0x81u8 -> 0x81u8
    between ~low:0x7fu8 ~high:0x81u8 0x81u8 -> true

    clamp ~min:0x7fu8 ~max:0x81u8 0x82u8 -> 0x81u8
    between ~low:0x7fu8 ~high:0x81u8 0x82u8 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  let fifteen = (kv 15) in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1u8 -> 0x00u8
    min_value - 1u8 -> 0xffu8
    max_value * 15u8 -> 0xf1u8
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
          Sint.pp_x i pp_x t Sint.pp i pp_x t';
        let t = of_uns (Uns.of_sint i) in
        let u = to_uns t in
        let t' = of_uns u in
        printf "of_uns %a -> to_uns %a -> of_uns %a -> %a\n"
          Sint.pp_x i pp_x t Uns.pp_x u pp_x t';

        let c = Codepoint.of_uns (Uns.of_sint i) in
        let t = of_codepoint c in
        let c' = to_codepoint t in
        let t' = of_codepoint c' in
        printf ("Codepoint.of_uns %a -> of_codepoint %a -> " ^^
            "to_codepoint %a -> of_codepoint %a -> %a\n") Uns.pp_x x
          Codepoint.pp_x c pp_x t Codepoint.pp_x c' pp_x t';

        fn xs'
      end
  in
  fn [Uns.max_value; 0; 42; 127; 128; 255; 256; 257;
    Uns.of_sint Sint.max_value];

  [%expect{|
    of_sint 0x7fffffffffffffffi -> to_sint 0xffu8 -> of_sint -1i -> 0xffu8
    of_uns 0x7fffffffffffffffi -> to_uns 0xffu8 -> of_uns 0x00000000000000ff -> 0xffu8
    Codepoint.of_uns 0x7fffffffffffffff -> of_codepoint 0x1fffffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    of_sint 0x0000000000000000i -> to_sint 0x00u8 -> of_sint 0i -> 0x00u8
    of_uns 0x0000000000000000i -> to_uns 0x00u8 -> of_uns 0x0000000000000000 -> 0x00u8
    Codepoint.of_uns 0x0000000000000000 -> of_codepoint 0x000000u21 -> to_codepoint 0x00u8 -> of_codepoint 0x000000u21 -> 0x00u8
    of_sint 0x000000000000002ai -> to_sint 0x2au8 -> of_sint 42i -> 0x2au8
    of_uns 0x000000000000002ai -> to_uns 0x2au8 -> of_uns 0x000000000000002a -> 0x2au8
    Codepoint.of_uns 0x000000000000002a -> of_codepoint 0x00002au21 -> to_codepoint 0x2au8 -> of_codepoint 0x00002au21 -> 0x2au8
    of_sint 0x000000000000007fi -> to_sint 0x7fu8 -> of_sint 127i -> 0x7fu8
    of_uns 0x000000000000007fi -> to_uns 0x7fu8 -> of_uns 0x000000000000007f -> 0x7fu8
    Codepoint.of_uns 0x000000000000007f -> of_codepoint 0x00007fu21 -> to_codepoint 0x7fu8 -> of_codepoint 0x00007fu21 -> 0x7fu8
    of_sint 0x0000000000000080i -> to_sint 0x80u8 -> of_sint 128i -> 0x80u8
    of_uns 0x0000000000000080i -> to_uns 0x80u8 -> of_uns 0x0000000000000080 -> 0x80u8
    Codepoint.of_uns 0x0000000000000080 -> of_codepoint 0x000080u21 -> to_codepoint 0x80u8 -> of_codepoint 0x000080u21 -> 0x80u8
    of_sint 0x00000000000000ffi -> to_sint 0xffu8 -> of_sint 255i -> 0xffu8
    of_uns 0x00000000000000ffi -> to_uns 0xffu8 -> of_uns 0x00000000000000ff -> 0xffu8
    Codepoint.of_uns 0x00000000000000ff -> of_codepoint 0x0000ffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    of_sint 0x0000000000000100i -> to_sint 0x00u8 -> of_sint 256i -> 0x00u8
    of_uns 0x0000000000000100i -> to_uns 0x00u8 -> of_uns 0x0000000000000000 -> 0x00u8
    Codepoint.of_uns 0x0000000000000100 -> of_codepoint 0x000100u21 -> to_codepoint 0x00u8 -> of_codepoint 0x000000u21 -> 0x00u8
    of_sint 0x0000000000000101i -> to_sint 0x01u8 -> of_sint 257i -> 0x01u8
    of_uns 0x0000000000000101i -> to_uns 0x01u8 -> of_uns 0x0000000000000001 -> 0x01u8
    Codepoint.of_uns 0x0000000000000101 -> of_codepoint 0x000101u21 -> to_codepoint 0x01u8 -> of_codepoint 0x000001u21 -> 0x01u8
    of_sint 0x3fffffffffffffffi -> to_sint 0xffu8 -> of_sint 4611686018427387903i -> 0xffu8
    of_uns 0x3fffffffffffffffi -> to_uns 0xffu8 -> of_uns 0x00000000000000ff -> 0xffu8
    Codepoint.of_uns 0x3fffffffffffffff -> of_codepoint 0x1fffffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
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

    0x1p7;
    0x1p8;
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
    of_float -0x1p+0 -> 0x00u8; to_float -> 0x0p+0
    of_float 0x0p+0 -> 0x00u8; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x00u8; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x01u8; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0xffu8; to_float -> 0x1.fep+7
    of_float 0x1.fffffffffffffp+52 -> 0xffu8; to_float -> 0x1.fep+7
    of_float 0x1.fffffffffffffp+56 -> 0xf0u8; to_float -> 0x1.ep+7
    of_float 0x1.fffffffffffffp+61 -> 0x00u8; to_float -> 0x0p+0
    of_float 0x1p+7 -> 0x80u8; to_float -> 0x1p+7
    of_float 0x1p+8 -> 0x00u8; to_float -> 0x0p+0

    to_float 0x00u8 -> 0x0p+0; of_float -> 0x00u8
    to_float 0x01u8 -> 0x1p+0; of_float -> 0x01u8
    to_float 0xffu8 -> 0x1.fep+7; of_float -> 0xffu8
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
    (kv 0xff, kv 0);
    (kv 0, kv 0xff);
    (kv 0xff, kv 0xff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x00u8 0x00u8 -> 0x00u8, 0x00u8, 0x00u8
    bit_{and,or,xor} 0xffu8 0x00u8 -> 0x00u8, 0xffu8, 0xffu8
    bit_{and,or,xor} 0x00u8 0xffu8 -> 0x00u8, 0xffu8, 0xffu8
    bit_{and,or,xor} 0xffu8 0xffu8 -> 0xffu8, 0xffu8, 0x00u8
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
    kv 0xff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x00u8 -> 0xffu8
    bit_not 0xffu8 -> 0x00u8
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
    kv 0x80;
    kv 0xff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x00u8 -> 0, 8, 8
    bit_{pop,clz,ctz} 0x01u8 -> 1, 7, 0
    bit_{pop,clz,ctz} 0x80u8 -> 1, 0, 7
    bit_{pop,clz,ctz} 0xffu8 -> 8, 0, 0
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

    (0xff, 0);
    (0xff, 1);

    (2, 3);
    (2, 4);
    (2, 7);
    (2, 8);

    (0xf, 0xf);
    (0xff, 0xff);

    (1, 0xff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x00u8 ** 0x00u8 -> 0x01u8
    0x00u8 ** 0x01u8 -> 0x00u8
    0xffu8 ** 0x00u8 -> 0x01u8
    0xffu8 ** 0x01u8 -> 0xffu8
    0x02u8 ** 0x03u8 -> 0x08u8
    0x02u8 ** 0x04u8 -> 0x10u8
    0x02u8 ** 0x07u8 -> 0x80u8
    0x02u8 ** 0x08u8 -> 0x00u8
    0x0fu8 ** 0x0fu8 -> 0xefu8
    0xffu8 ** 0xffu8 -> 0xffu8
    0x01u8 ** 0xffu8 -> 0x01u8
    |}]
