include Rudiments_int

let to_sint t =
  sint_of_uns t

let of_sint x =
  uns_of_sint x

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
    hash_fold 0x0000000000000000 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x0000000000000001 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x0000000000000000 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x7fffffffffffffff -> 0x913b_a441_dcb5_f088_efa8_6f78_1580_b321u128
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=63
    min_value=0x0000000000000000
    max_value=0x7fffffffffffffff
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
  fn 0 0x4000_0000_0000_0000;
  printf "\n";
  fn 0 0x7fff_ffff_ffff_ffff;
  printf "\n";
  fn 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff;
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 0x3fff_ffff_ffff_fffe 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x3fff_ffff_ffff_ffff 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0001 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0002 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;

  [%expect{|
    cmp 0x0000000000000000 0x4000000000000000 -> Lt
    0x0000000000000000 >= 0x4000000000000000 -> false
    0x0000000000000000 <= 0x4000000000000000 -> true
    0x0000000000000000 = 0x4000000000000000 -> false
    0x0000000000000000 > 0x4000000000000000 -> false
    0x0000000000000000 < 0x4000000000000000 -> true
    0x0000000000000000 <> 0x4000000000000000 -> true
    ascending 0x0000000000000000 0x4000000000000000 -> Lt
    descending 0x0000000000000000 0x4000000000000000 -> Gt

    cmp 0x0000000000000000 0x7fffffffffffffff -> Lt
    0x0000000000000000 >= 0x7fffffffffffffff -> false
    0x0000000000000000 <= 0x7fffffffffffffff -> true
    0x0000000000000000 = 0x7fffffffffffffff -> false
    0x0000000000000000 > 0x7fffffffffffffff -> false
    0x0000000000000000 < 0x7fffffffffffffff -> true
    0x0000000000000000 <> 0x7fffffffffffffff -> true
    ascending 0x0000000000000000 0x7fffffffffffffff -> Lt
    descending 0x0000000000000000 0x7fffffffffffffff -> Gt

    cmp 0x4000000000000000 0x3fffffffffffffff -> Gt
    0x4000000000000000 >= 0x3fffffffffffffff -> true
    0x4000000000000000 <= 0x3fffffffffffffff -> false
    0x4000000000000000 = 0x3fffffffffffffff -> false
    0x4000000000000000 > 0x3fffffffffffffff -> true
    0x4000000000000000 < 0x3fffffffffffffff -> false
    0x4000000000000000 <> 0x3fffffffffffffff -> true
    ascending 0x4000000000000000 0x3fffffffffffffff -> Gt
    descending 0x4000000000000000 0x3fffffffffffffff -> Lt

    clamp ~min:0x3fffffffffffffff ~max:0x4000000000000001 0x3ffffffffffffffe -> 0x3fffffffffffffff
    between ~low:0x3fffffffffffffff ~high:0x4000000000000001 0x3ffffffffffffffe -> false

    clamp ~min:0x3fffffffffffffff ~max:0x4000000000000001 0x3fffffffffffffff -> 0x3fffffffffffffff
    between ~low:0x3fffffffffffffff ~high:0x4000000000000001 0x3fffffffffffffff -> true

    clamp ~min:0x3fffffffffffffff ~max:0x4000000000000001 0x4000000000000000 -> 0x4000000000000000
    between ~low:0x3fffffffffffffff ~high:0x4000000000000001 0x4000000000000000 -> true

    clamp ~min:0x3fffffffffffffff ~max:0x4000000000000001 0x4000000000000001 -> 0x4000000000000001
    between ~low:0x3fffffffffffffff ~high:0x4000000000000001 0x4000000000000001 -> true

    clamp ~min:0x3fffffffffffffff ~max:0x4000000000000001 0x4000000000000002 -> 0x4000000000000001
    between ~low:0x3fffffffffffffff ~high:0x4000000000000001 0x4000000000000002 -> false
    |}]

let%expect_test "narrowing" =
  let open Format in
  printf "max_value + 1 -> %a\n" pp_x (max_value + 1);
  printf "min_value - 1 -> %a\n" pp_x (min_value - 1);
  printf "max_value * 15 -> %a\n" pp_x (max_value * 15);

  [%expect{|
    max_value + 1 -> 0x0000000000000000
    min_value - 1 -> 0x7fffffffffffffff
    max_value * 15 -> 0x7ffffffffffffff1
    |}]

let%expect_test "of_real,to_real" =
  let open Format in
  printf "@[<h>";
  let rec test_rs rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        let x = of_real r in
        printf "of_real %h -> %a; to_real -> %h\n"
          r pp_x x (to_real x);
        test_rs rs'
      end
  end in
  let rs = [
    -1.;
    0.;
    0x1.1p-1;
    1.;

    0x1.f_ffff_ffff_ffffp48;
    0x1.f_ffff_ffff_ffffp52;
    0x1.f_ffff_ffff_ffffp56;

    0x1.f_ffff_ffff_ffffp61;

    0x1p60;
    0x1p61;
  ] in
  test_rs rs;
  printf "\n";
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let r = to_real x in
        printf "to_real %a -> %h; of_real -> %a\n"
          pp_x x r pp_x (of_real r);
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
    of_real -0x1p+0 -> 0x0000000000000000; to_real -> 0x0p+0
    of_real 0x0p+0 -> 0x0000000000000000; to_real -> 0x0p+0
    of_real 0x1.1p-1 -> 0x0000000000000000; to_real -> 0x0p+0
    of_real 0x1p+0 -> 0x0000000000000001; to_real -> 0x1p+0
    of_real 0x1.fffffffffffffp+48 -> 0x0001ffffffffffff; to_real -> 0x1.ffffffffffffp+48
    of_real 0x1.fffffffffffffp+52 -> 0x001fffffffffffff; to_real -> 0x1.fffffffffffffp+52
    of_real 0x1.fffffffffffffp+56 -> 0x01fffffffffffff0; to_real -> 0x1.fffffffffffffp+56
    of_real 0x1.fffffffffffffp+61 -> 0x3ffffffffffffe00; to_real -> 0x1.fffffffffffffp+61
    of_real 0x1p+60 -> 0x1000000000000000; to_real -> 0x1p+60
    of_real 0x1p+61 -> 0x2000000000000000; to_real -> 0x1p+61

    to_real 0x0000000000000000 -> 0x0p+0; of_real -> 0x0000000000000000
    to_real 0x0000000000000001 -> 0x1p+0; of_real -> 0x0000000000000001
    to_real 0x7fffffffffffffff -> 0x1p+63; of_real -> 0x0000000000000000
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
    (0, 0);
    (0x7fff_ffff_ffff_ffff, 0);
    (0, 0x7fff_ffff_ffff_ffff);
    (0x7fff_ffff_ffff_ffff, 0x7fff_ffff_ffff_ffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0000000000000000 0x0000000000000000 -> 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
    bit_{and,or,xor} 0x7fffffffffffffff 0x0000000000000000 -> 0x0000000000000000, 0x7fffffffffffffff, 0x7fffffffffffffff
    bit_{and,or,xor} 0x0000000000000000 0x7fffffffffffffff -> 0x0000000000000000, 0x7fffffffffffffff, 0x7fffffffffffffff
    bit_{and,or,xor} 0x7fffffffffffffff 0x7fffffffffffffff -> 0x7fffffffffffffff, 0x7fffffffffffffff, 0x0000000000000000
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
    0;
    0x7fff_ffff_ffff_ffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0000000000000000 -> 0x7fffffffffffffff
    bit_not 0x7fffffffffffffff -> 0x0000000000000000
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
    0;
    1;
    0x4000_0000_0000_0000;
    0x7fff_ffff_ffff_ffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x0000000000000000 -> 0, 63, 63
    bit_{pop,clz,ctz} 0x0000000000000001 -> 1, 62, 0
    bit_{pop,clz,ctz} 0x4000000000000000 -> 1, 0, 62
    bit_{pop,clz,ctz} 0x7fffffffffffffff -> 63, 0, 0
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

    (0x7fff_ffff_ffff_ffff, 0);
    (0x7fff_ffff_ffff_ffff, 1);

    (2, 31);
    (2, 32);
    (2, 62);
    (2, 63);

    (0xf, 0xf);
    (0xff, 0xff);

    (1, 0x7fff_ffff_ffff_ffff);

    (0x7fff_ffff_ffff_ffff, 0x7fff_ffff_ffff_ffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000000000000000 ** 0x0000000000000000 -> 0x0000000000000001
    0x0000000000000000 ** 0x0000000000000001 -> 0x0000000000000000
    0x7fffffffffffffff ** 0x0000000000000000 -> 0x0000000000000001
    0x7fffffffffffffff ** 0x0000000000000001 -> 0x7fffffffffffffff
    0x0000000000000002 ** 0x000000000000001f -> 0x0000000080000000
    0x0000000000000002 ** 0x0000000000000020 -> 0x0000000100000000
    0x0000000000000002 ** 0x000000000000003e -> 0x4000000000000000
    0x0000000000000002 ** 0x000000000000003f -> 0x0000000000000000
    0x000000000000000f ** 0x000000000000000f -> 0x0613b62c597707ef
    0x00000000000000ff ** 0x00000000000000ff -> 0x5997756b007ffeff
    0x0000000000000001 ** 0x7fffffffffffffff -> 0x0000000000000001
    0x7fffffffffffffff ** 0x7fffffffffffffff -> 0x7fffffffffffffff
    |}]
