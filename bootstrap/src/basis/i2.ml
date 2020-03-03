(* Partial Rudiments. *)
module Isize = I63
module Usize = U63
open Rudiments_int
open Rudiments_functions

module T = struct
  type t = isize
  let num_bits = 2
end
include T
include Intnb.Make_i(T)

let to_isize t =
  t

let of_isize x =
  narrow_of_signed x

let of_isize_hlt x =
  let t = of_isize x in
  let x' = to_isize t in
  match Isize.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let kv x =
  narrow_of_signed (isize_of_int x)

let to_usize t =
  usize_of_isize t

let of_usize x =
  narrow_of_unsigned x

let of_usize_hlt x =
  let t = of_usize x in
  let x' = to_usize t in
  match Usize.(x' = x) with
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
    hash_fold 0x2i2 -> 0x25a5_4a95_c972_81d9_817c_f634_2c95_a7deu128
    hash_fold 0x3i2 -> 0x913b_a441_dcb5_f088_efa8_6f78_1580_b321u128
    hash_fold 0x0i2 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x1i2 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x1i2 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
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
    -2i2 0x2i2
    -1i2 0x3i2
    0i2 0x0i2
    1i2 0x1i2
    1i2 0x1i2
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Usize.pp num_bits;
  printf "min_value=%a %a\n" pp min_value pp_x min_value;
  printf "max_value=%a %a\n" pp max_value pp_x max_value;

  [%expect{|
    num_bits=2
    min_value=-2i2 0x2i2
    max_value=1i2 0x1i2
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
  fn (kv 0) (kv (-2));
  printf "\n";
  fn (kv 0) (kv (-1));
  printf "\n";
  fn (kv (-2)) (kv (-1));
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
    cmp 0x0i2 0x2i2 -> Gt
    0x0i2 >= 0x2i2 -> true
    0x0i2 <= 0x2i2 -> false
    0x0i2 = 0x2i2 -> false
    0x0i2 > 0x2i2 -> true
    0x0i2 < 0x2i2 -> false
    0x0i2 <> 0x2i2 -> true
    ascending 0x0i2 0x2i2 -> Gt
    descending 0x0i2 0x2i2 -> Lt

    cmp 0x0i2 0x3i2 -> Gt
    0x0i2 >= 0x3i2 -> true
    0x0i2 <= 0x3i2 -> false
    0x0i2 = 0x3i2 -> false
    0x0i2 > 0x3i2 -> true
    0x0i2 < 0x3i2 -> false
    0x0i2 <> 0x3i2 -> true
    ascending 0x0i2 0x3i2 -> Gt
    descending 0x0i2 0x3i2 -> Lt

    cmp 0x2i2 0x3i2 -> Lt
    0x2i2 >= 0x3i2 -> false
    0x2i2 <= 0x3i2 -> true
    0x2i2 = 0x3i2 -> false
    0x2i2 > 0x3i2 -> false
    0x2i2 < 0x3i2 -> true
    0x2i2 <> 0x3i2 -> true
    ascending 0x2i2 0x3i2 -> Lt
    descending 0x2i2 0x3i2 -> Gt

    clamp ~min:0x3i2 ~max:0x0i2 0x2i2 -> 0x3i2
    between ~low:0x3i2 ~high:0x0i2 0x2i2 -> false

    clamp ~min:0x3i2 ~max:0x0i2 0x3i2 -> 0x3i2
    between ~low:0x3i2 ~high:0x0i2 0x3i2 -> true

    clamp ~min:0x3i2 ~max:0x0i2 0x0i2 -> 0x0i2
    between ~low:0x3i2 ~high:0x0i2 0x0i2 -> true

    clamp ~min:0x3i2 ~max:0x0i2 0x1i2 -> 0x0i2
    between ~low:0x3i2 ~high:0x0i2 0x1i2 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);

  [%expect{|
    max_value + 1i2 -> 0x2i2
    min_value - 1i2 -> 0x1i2
    |}]

(*
let%expect_test "conversion" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = isize_of_int x in
        let t = of_isize i in
        let i' = to_isize t in
        let t' = of_isize i' in
        printf "of_isize %a -> to_isize %a -> of_isize %a -> %a\n"
          Isize.pp_x i pp_x t Isize.pp i pp_x t';
        let t = of_usize (Usize.of_isize i) in
        let u = to_usize t in
        let t' = of_usize u in
        printf "of_usize %a -> to_usize %a -> of_usize %a -> %a\n"
          Isize.pp_x i pp_x t Usize.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Usize.max_value; (-2); (-1); 0; 1; 2; Usize.of_isize Isize.max_value];

  [%expect{|
    of_isize 0x7fffffffffffffffi -> to_isize 0xffu8 -> of_isize -1i -> 0xffu8
    of_usize 0x7fffffffffffffffi -> to_usize 0xffu8 -> of_usize 0x00000000000000ff -> 0xffu8
    Codepoint.of_usize 0x7fffffffffffffff -> of_codepoint 0x1fffffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    of_isize 0x0000000000000000i -> to_isize 0x00u8 -> of_isize 0i -> 0x00u8
    of_usize 0x0000000000000000i -> to_usize 0x00u8 -> of_usize 0x0000000000000000 -> 0x00u8
    Codepoint.of_usize 0x0000000000000000 -> of_codepoint 0x000000u21 -> to_codepoint 0x00u8 -> of_codepoint 0x000000u21 -> 0x00u8
    of_isize 0x000000000000002ai -> to_isize 0x2au8 -> of_isize 42i -> 0x2au8
    of_usize 0x000000000000002ai -> to_usize 0x2au8 -> of_usize 0x000000000000002a -> 0x2au8
    Codepoint.of_usize 0x000000000000002a -> of_codepoint 0x00002au21 -> to_codepoint 0x2au8 -> of_codepoint 0x00002au21 -> 0x2au8
    of_isize 0x000000000000007fi -> to_isize 0x7fu8 -> of_isize 127i -> 0x7fu8
    of_usize 0x000000000000007fi -> to_usize 0x7fu8 -> of_usize 0x000000000000007f -> 0x7fu8
    Codepoint.of_usize 0x000000000000007f -> of_codepoint 0x00007fu21 -> to_codepoint 0x7fu8 -> of_codepoint 0x00007fu21 -> 0x7fu8
    of_isize 0x0000000000000080i -> to_isize 0x80u8 -> of_isize 128i -> 0x80u8
    of_usize 0x0000000000000080i -> to_usize 0x80u8 -> of_usize 0x0000000000000080 -> 0x80u8
    Codepoint.of_usize 0x0000000000000080 -> of_codepoint 0x000080u21 -> to_codepoint 0x80u8 -> of_codepoint 0x000080u21 -> 0x80u8
    of_isize 0x00000000000000ffi -> to_isize 0xffu8 -> of_isize 255i -> 0xffu8
    of_usize 0x00000000000000ffi -> to_usize 0xffu8 -> of_usize 0x00000000000000ff -> 0xffu8
    Codepoint.of_usize 0x00000000000000ff -> of_codepoint 0x0000ffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    of_isize 0x0000000000000100i -> to_isize 0x00u8 -> of_isize 256i -> 0x00u8
    of_usize 0x0000000000000100i -> to_usize 0x00u8 -> of_usize 0x0000000000000000 -> 0x00u8
    Codepoint.of_usize 0x0000000000000100 -> of_codepoint 0x000100u21 -> to_codepoint 0x00u8 -> of_codepoint 0x000000u21 -> 0x00u8
    of_isize 0x0000000000000101i -> to_isize 0x01u8 -> of_isize 257i -> 0x01u8
    of_usize 0x0000000000000101i -> to_usize 0x01u8 -> of_usize 0x0000000000000001 -> 0x01u8
    Codepoint.of_usize 0x0000000000000101 -> of_codepoint 0x000101u21 -> to_codepoint 0x01u8 -> of_codepoint 0x000001u21 -> 0x01u8
    of_isize 0x3fffffffffffffffi -> to_isize 0xffu8 -> of_isize 4611686018427387903i -> 0xffu8
    of_usize 0x3fffffffffffffffi -> to_usize 0xffu8 -> of_usize 0x00000000000000ff -> 0xffu8
    Codepoint.of_usize 0x3fffffffffffffff -> of_codepoint 0x1fffffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    |}]
*)

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

    0x1p0;
    0x1p1;
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
    of_float -0x1p+0 -> 0x3i2; to_float -> -0x1p+0
    of_float 0x0p+0 -> 0x0i2; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x0i2; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x1i2; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0x1i2; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+52 -> 0x1i2; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+56 -> 0x0i2; to_float -> 0x0p+0
    of_float 0x1.fffffffffffffp+61 -> 0x0i2; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x1i2; to_float -> 0x1p+0
    of_float 0x1p+1 -> 0x0i2; to_float -> 0x0p+0

    to_float 0x3i2 -> -0x1p+0; of_float -> 0x3i2
    to_float 0x0i2 -> 0x0p+0; of_float -> 0x0i2
    to_float 0x1i2 -> 0x1p+0; of_float -> 0x1i2
    to_float 0x1i2 -> 0x1p+0; of_float -> 0x1i2
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
    (kv 0x3, kv 0);
    (kv 0, kv 0x3);
    (kv 0x3, kv 0x3);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0i2 0x0i2 -> 0x0i2, 0x0i2, 0x0i2
    bit_{and,or,xor} 0x1i2 0x0i2 -> 0x0i2, 0x1i2, 0x1i2
    bit_{and,or,xor} 0x0i2 0x1i2 -> 0x0i2, 0x1i2, 0x1i2
    bit_{and,or,xor} 0x1i2 0x1i2 -> 0x1i2, 0x1i2, 0x0i2
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
    kv 0x3
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0i2 -> 0x3i2
    bit_not 0x1i2 -> 0x2i2
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
    kv (-2);
    kv (-1);
    kv 0;
    kv 1;
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x2i2 -> 1, 0, 1
    bit_{pop,clz,ctz} 0x3i2 -> 2, 0, 0
    bit_{pop,clz,ctz} 0x0i2 -> 0, 2, 2
    bit_{pop,clz,ctz} 0x1i2 -> 1, 1, 0
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
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0i2 ** 0x0i2 -> 0x1i2
    0x0i2 ** 0x1i2 -> 0x0i2
    0x1i2 ** 0x0i2 -> 0x1i2
    0x1i2 ** 0x1i2 -> 0x1i2
    |}]
