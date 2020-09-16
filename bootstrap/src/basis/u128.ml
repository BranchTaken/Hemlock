open RudimentsInt

module T = struct
  module U = struct
    type t = u128
    let num_bits = 128
    let of_arr a =
      u128_of_arr a
    let to_arr t =
      match u128_to_tup t with (lo, hi) -> [|lo; hi|]
  end
  include U
  include Intnw.MakeU(U)
end
include T
include Identifiable.Make(T)
include Cmpable.MakeZero(T)

let to_tup = u128_to_tup

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
  fn zero (of_string "0x8000_0000_0000_0000_0000_0000_0000_0000");
  printf "\n";
  fn zero (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string "0x8000_0000_0000_0000_0000_0000_0000_0000")
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_fffe")
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0000_0000_0000_0000_0000")
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0000_0000_0000_0000_0001")
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0000_0000_0000_0000_0002")
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0000_0000_0000_0000_0001");

  [%expect{|
    cmp 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> Lt
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 >= 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <= 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 = 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 > 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 < 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <> 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> true
    ascending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> Lt
    descending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> Gt

    cmp 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> Lt
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 >= 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <= 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 = 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 > 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 < 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000u128 <> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> true
    ascending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> Lt
    descending 0x0000_0000_0000_0000_0000_0000_0000_0000u128 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> Gt

    cmp 0x8000_0000_0000_0000_0000_0000_0000_0000u128 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> Gt
    0x8000_0000_0000_0000_0000_0000_0000_0000u128 >= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> true
    0x8000_0000_0000_0000_0000_0000_0000_0000u128 <= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000u128 = 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000u128 > 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> true
    0x8000_0000_0000_0000_0000_0000_0000_0000u128 < 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000u128 <> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> true
    ascending 0x8000_0000_0000_0000_0000_0000_0000_0000u128 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> Gt
    descending 0x8000_0000_0000_0000_0000_0000_0000_0000u128 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> Lt

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~max:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_fffeu128 -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~high:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_fffeu128 -> false

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~max:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~high:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~max:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x8000_0000_0000_0000_0000_0000_0000_0000u128
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~high:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x8000_0000_0000_0000_0000_0000_0000_0000u128 -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~max:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x8000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x8000_0000_0000_0000_0000_0000_0000_0001u128
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~high:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x8000_0000_0000_0000_0000_0000_0000_0001u128 -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~max:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x8000_0000_0000_0000_0000_0000_0000_0002u128 -> 0x8000_0000_0000_0000_0000_0000_0000_0001u128
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 ~high:0x8000_0000_0000_0000_0000_0000_0000_0001u128 0x8000_0000_0000_0000_0000_0000_0000_0002u128 -> false
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

    0x1.f_ffff_ffff_ffffp127;
    0x1.f_ffff_ffff_ffffp128;
    0x1.f_ffff_ffff_ffffp132;

    0x1p126;
    0x1p127;
    0x1p128;
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
    of_real -0x1p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_real -> 0x0p+0
    of_real 0x0p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_real -> 0x0p+0
    of_real 0x1.1p-1 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_real -> 0x0p+0
    of_real 0x1p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128; to_real -> 0x1p+0
    of_real 0x1.fffffffffffffp+48 -> 0x0000_0000_0000_0000_0001_ffff_ffff_ffffu128; to_real -> 0x1.ffffffffffffp+48
    of_real 0x1.fffffffffffffp+52 -> 0x0000_0000_0000_0000_001f_ffff_ffff_ffffu128; to_real -> 0x1.fffffffffffffp+52
    of_real 0x1.fffffffffffffp+56 -> 0x0000_0000_0000_0000_01ff_ffff_ffff_fff0u128; to_real -> 0x1.fffffffffffffp+56
    of_real 0x1.fffffffffffffp+127 -> 0xffff_ffff_ffff_f800_0000_0000_0000_0000u128; to_real -> 0x1.fffffffffffffp+127
    of_real 0x1.fffffffffffffp+128 -> 0xffff_ffff_ffff_f000_0000_0000_0000_0000u128; to_real -> 0x1.ffffffffffffep+127
    of_real 0x1.fffffffffffffp+132 -> 0xffff_ffff_ffff_0000_0000_0000_0000_0000u128; to_real -> 0x1.fffffffffffep+127
    of_real 0x1p+126 -> 0x4000_0000_0000_0000_0000_0000_0000_0000u128; to_real -> 0x1p+126
    of_real 0x1p+127 -> 0x8000_0000_0000_0000_0000_0000_0000_0000u128; to_real -> 0x1p+127
    of_real 0x1p+128 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128; to_real -> 0x0p+0

    to_real 0x0000_0000_0000_0000_0000_0000_0000_0000u128 -> 0x0p+0; of_real -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    to_real 0x0000_0000_0000_0000_0000_0000_0000_0001u128 -> 0x1p+0; of_real -> 0x0000_0000_0000_0000_0000_0000_0000_0001u128
    to_real 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 -> 0x1.fffffffffffffp+127; of_real -> 0xffff_ffff_ffff_f800_0000_0000_0000_0000u128
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
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128 /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffffu128 -> 0x0000_0000_0000_0001_0000_0000_0000_0001u128, 0x0000_0000_0000_0000_0000_0000_0000_0000u128
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
