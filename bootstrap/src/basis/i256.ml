open Rudiments_int
open Rudiments_functions

module T = struct
  type t = u64 array
  let num_bits = 256
  let of_arr a = a
  let to_arr t = t
end
include T
include Intnw.Make_i(T)

let to_tup = function
  | [|w0; w1; w2; w3|] -> (w0, w1, w2, w3)
  | _ -> not_reached ()

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
    0i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    1i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    42i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_002ai256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
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
    hash_fold 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x10c0_1d1b_b334_2d3c_3172_3c28_4ade_5cd0u128
    hash_fold 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0xeb9f_c6c4_25fe_306a_6957_1e92_1a0d_5dcdu128
    hash_fold 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x10c0_1d1b_b334_2d3c_3172_3c28_4ade_5cd0u128
    hash_fold 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x6c23_2c39_aa5d_9f16_7592_e77d_c551_13f1u128
    |}]

let%expect_test "constants" =
  let open Format in

  printf "zero=%a %a\n" pp_x zero pp zero;
  printf "one=%a %a\n" pp_x one pp one;
  printf "min_value=%a %a\n" pp_x min_value pp min_value;
  printf "max_value=%a %a\n" pp_x max_value pp max_value;

  [%expect{|
    zero=0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0i256
    one=0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 1i256
    min_value=0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256
    max_value=0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256
    |}]

let%expect_test "of_string" =
  let open Format in
  printf "@[<h>";
  let rec test_strs = function
    | [] -> ()
    | s :: strs' -> begin
        printf "of_string %S -> %a %a\n" s pp (of_string s) pp_x (of_string s);
        test_strs strs'
      end
  in
  let strs = [
    "-1";
    "0";
    "1";
    "+1";
    "9876543210";
    "9876543210_";
    "9i256";
    "9_i256";
    "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    "0b0";
    "0b1";
    "0b10";
    "0b10_";
    "0b1i256";
    "0b_1_i256";
    ("0b_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111");

    "0x0";
    "0x1";
    "0xfedcba9876543210";
    "0xfedcba9876543210_";
    "0xfi256";
    "0x_f_i256";
    "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";
  ] in
  test_strs strs;
  printf "@]";

  [%expect{|
    of_string "-1" -> -1i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    of_string "0" -> 0i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    of_string "1" -> 1i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    of_string "+1" -> 1i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    of_string "9876543210" -> 9_876_543_210i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002_4cb0_16eai256
    of_string "9876543210_" -> 9_876_543_210i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002_4cb0_16eai256
    of_string "9i256" -> 9i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0009i256
    of_string "9_i256" -> 9i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0009i256
    of_string "115792089237316195423570985008687907853269984665640564039457584007913129639935" -> -1i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    of_string "0b0" -> 0i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    of_string "0b1" -> 1i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    of_string "0b10" -> 2i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256
    of_string "0b10_" -> 2i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256
    of_string "0b1i256" -> 1i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    of_string "0b_1_i256" -> 1i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    of_string "0b_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111" -> -1i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    of_string "0x0" -> 0i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    of_string "0x1" -> 1i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    of_string "0xfedcba9876543210" -> 18_364_758_544_493_064_720i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_fedc_ba98_7654_3210i256
    of_string "0xfedcba9876543210_" -> 18_364_758_544_493_064_720i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_fedc_ba98_7654_3210i256
    of_string "0xfi256" -> 15i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_000fi256
    of_string "0x_f_i256" -> 15i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_000fi256
    of_string "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff" -> -1i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
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
  fn zero (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000");
  printf "\n";
  fn zero (of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp min pp max pp t pp (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp min pp max pp t (between ~low:min ~high:max t);
  end in
  fn2 (of_string "-2") (of_string "-1") (of_string "1");
  fn2 (of_string "1") (of_string "1") (of_string "1");
  fn2 (of_string "0") (of_string "-1") (of_string "1");
  fn2 (of_string "1") (of_string "-1") (of_string "1");
  fn2 (of_string "2") (of_string "-1") (of_string "1");

  [%expect{|
    cmp 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> Gt
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 >= 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 <= 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 = 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 > 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 < 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 <> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> true
    ascending 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> Gt
    descending 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> Lt

    cmp 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> Gt
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 >= 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 <= 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 = 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 > 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> true
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 < 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> false
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 <> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> true
    ascending 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> Gt
    descending 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> Lt

    cmp 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> Lt
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 >= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 <= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> true
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 = 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 > 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 < 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> true
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 <> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> true
    ascending 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> Lt
    descending 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> Gt

    clamp ~min:-1i256 ~max:1i256 -2i256 -> -1i256
    between ~low:-1i256 ~high:1i256 -2i256 -> false

    clamp ~min:1i256 ~max:1i256 1i256 -> 1i256
    between ~low:1i256 ~high:1i256 1i256 -> true

    clamp ~min:-1i256 ~max:1i256 0i256 -> 0i256
    between ~low:-1i256 ~high:1i256 0i256 -> true

    clamp ~min:-1i256 ~max:1i256 1i256 -> 1i256
    between ~low:-1i256 ~high:1i256 1i256 -> true

    clamp ~min:-1i256 ~max:1i256 2i256 -> 1i256
    between ~low:-1i256 ~high:1i256 2i256 -> false
    |}]

let%expect_test "narrowing" =
  let open Format in
  let fifteen = of_string "15" in
  printf "neg_one + one -> %a\n" pp_x (neg_one + one);
  printf "zero - one -> %a\n" pp_x (zero - one);
  printf "neg_one * %a -> %a\n" pp fifteen pp_x (neg_one * fifteen);

  [%expect{|
    neg_one + one -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    zero - one -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    neg_one * 15i256 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fff1i256
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

    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffffi256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001_0000_0000_0000_0000i256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_0000_0000_0000_0002i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffffi256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_fffei256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x0000_0000_0000_0000_0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000i256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0002i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffei256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 +,- 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 +,- 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffei256
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
    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 * 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 * 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 * 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffffi256 * 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffffi256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_fffe_0000_0001i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffffi256 * 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffffi256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_fffe_0000_0000_0000_0001i256
    0x0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 * 0x0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe_0000_0000_0000_0000_0000_0000_0000_0001i256
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 * 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
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

    0x1.f_ffff_ffff_ffffp254;
    0x1.f_ffff_ffff_ffffp255;
    0x1.f_ffff_ffff_ffffp256;
    0x1.f_ffff_ffff_ffffp260;

    0x1p253;
    0x1p254;
    0x1p255;
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
  let two = one + one in
  let xs = [
    zero;
    one;
    two;
    min_value / two;
    min_value;
    max_value;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_real -0x1p+0 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256; to_real -> -0x1p+0
    of_real 0x0p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x0p+0
    of_real 0x1.1p-1 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x0p+0
    of_real 0x1p+0 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256; to_real -> 0x1p+0
    of_real 0x1.fffffffffffffp+48 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001_ffff_ffff_ffffi256; to_real -> 0x1.ffffffffffffp+48
    of_real 0x1.fffffffffffffp+52 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_001f_ffff_ffff_ffffi256; to_real -> 0x1.fffffffffffffp+52
    of_real 0x1.fffffffffffffp+56 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_01ff_ffff_ffff_fff0i256; to_real -> 0x1.fffffffffffffp+56
    of_real 0x1.fffffffffffffp+127 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_ffff_ffff_ffff_f800_0000_0000_0000_0000i256; to_real -> 0x1.fffffffffffffp+127
    of_real 0x1.fffffffffffffp+128 -> 0x0000_0000_0000_0000_0000_0000_0000_0001_ffff_ffff_ffff_f000_0000_0000_0000_0000i256; to_real -> 0x1.fffffffffffffp+128
    of_real 0x1.fffffffffffffp+132 -> 0x0000_0000_0000_0000_0000_0000_0000_001f_ffff_ffff_ffff_0000_0000_0000_0000_0000i256; to_real -> 0x1.fffffffffffffp+132
    of_real 0x1.fffffffffffffp+254 -> 0x7fff_ffff_ffff_fc00_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x1.fffffffffffffp+254
    of_real 0x1.fffffffffffffp+255 -> 0x7fff_ffff_ffff_f800_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x1.ffffffffffffep+254
    of_real 0x1.fffffffffffffp+256 -> 0x7fff_ffff_ffff_f000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x1.ffffffffffffcp+254
    of_real 0x1.fffffffffffffp+260 -> 0x7fff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x1.fffffffffffcp+254
    of_real 0x1p+253 -> 0x2000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x1p+253
    of_real 0x1p+254 -> 0x4000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x1p+254
    of_real 0x1p+255 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256; to_real -> 0x0p+0

    to_real 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x0p+0; of_real -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    to_real 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x1p+0; of_real -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    to_real 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256 -> 0x1p+1; of_real -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256
    to_real 0xc000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> -0x1p+254; of_real -> 0xc000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    to_real 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> -0x1p+255; of_real -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    to_real 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x1.fffffffffffffp+254; of_real -> 0x7fff_ffff_ffff_fc00_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
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
          pp x pp y pp quotient pp remainder;
        assert (x = (y * quotient + remainder));
        test_pairs pairs'
      end
  in
  let pairs = [
    (* < 1 *)
    (of_string "0", of_string "1");

    (of_string "1", of_string "1");
    (of_string "-1", of_string "1");
    (of_string "1", of_string "-1");
    (of_string "-1", of_string "-1");

    (of_string "7", of_string "3");
    (of_string "-7", of_string "3");
    (of_string "7", of_string "-3");
    (of_string "-7", of_string "-3");

    (of_string "1", max_value);
    (max_value, of_string "1");

    (of_string "1", min_value);
    (min_value, of_string "1");

    (of_string "0xfffe", of_string "0xffff");
    (of_string "0xffff_fffe", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_fffe", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xf_ffff_ffff_ffff_fffe", of_string "0xf_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (max_value - one, max_value);

    (* Single-digit (base 2^32) divisor. *)
    (max_value, of_string "1");
    (max_value, of_string "2");
    (max_value, of_string "3");
    (max_value, of_string "7");
    (max_value, of_string "0xffff");
    (max_value, of_string "0xffff_ffff");

    (min_value, of_string "1");
    (min_value, of_string "-1"); (* Surprising sign due to overflow. *)
    (min_value, of_string "2");
    (min_value, of_string "-2");
    (min_value, of_string "3");
    (min_value, of_string "7");
    (min_value, of_string "0xffff");
    (min_value, of_string "0xffff_ffff");

    (* Multi-digit (base 2^32) divisor. *)
    (of_string "0x1_0000_0000", of_string "0x1_0000_0000");
    (of_string "0x1_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0x2_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (max_value, max_value);
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff");
    (max_value, of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0i256 /,% 1i256 -> 0i256, 0i256
    1i256 /,% 1i256 -> 1i256, 0i256
    -1i256 /,% 1i256 -> -1i256, 0i256
    1i256 /,% -1i256 -> -1i256, 0i256
    -1i256 /,% -1i256 -> 1i256, 0i256
    7i256 /,% 3i256 -> 2i256, 1i256
    -7i256 /,% 3i256 -> -2i256, -1i256
    7i256 /,% -3i256 -> -2i256, 1i256
    -7i256 /,% -3i256 -> 2i256, -1i256
    1i256 /,% 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 -> 0i256, 1i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 1i256 -> 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256, 0i256
    1i256 /,% -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 -> 0i256, 1i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% 1i256 -> -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256, 0i256
    65_534i256 /,% 65_535i256 -> 0i256, 65_534i256
    4_294_967_294i256 /,% 4_294_967_295i256 -> 0i256, 4_294_967_294i256
    18_446_744_073_709_551_614i256 /,% 18_446_744_073_709_551_615i256 -> 0i256, 18_446_744_073_709_551_614i256
    295_147_905_179_352_825_854i256 /,% 295_147_905_179_352_825_855i256 -> 0i256, 295_147_905_179_352_825_854i256
    1_208_925_819_614_629_174_706_174i256 /,% 1_208_925_819_614_629_174_706_175i256 -> 0i256, 1_208_925_819_614_629_174_706_174i256
    340_282_366_920_938_463_463_374_607_431_768_211_454i256 /,% 340_282_366_920_938_463_463_374_607_431_768_211_455i256 -> 0i256, 340_282_366_920_938_463_463_374_607_431_768_211_454i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_966i256 /,% 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 -> 0i256, 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_966i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 1i256 -> 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256, 0i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 2i256 -> 28_948_022_309_329_048_855_892_746_252_171_976_963_317_496_166_410_141_009_864_396_001_978_282_409_983i256, 1i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 3i256 -> 19_298_681_539_552_699_237_261_830_834_781_317_975_544_997_444_273_427_339_909_597_334_652_188_273_322i256, 1i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 7i256 -> 8_270_863_516_951_156_815_969_356_072_049_136_275_233_570_333_260_040_288_532_684_571_993_794_974_281i256, 0i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 65_535i256 -> 883_437_012_568_216_948_375_455_748_902_784_068_461_661_590_490_887_037_761_940_825_573_457_920i256, 32_767i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 4_294_967_295i256 -> 13_479_973_336_713_870_765_757_598_744_263_302_691_024_331_148_677_648_317_163_446_992_896i256, 2_147_483_647i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% 1i256 -> -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256, 0i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% -1i256 -> -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256, 0i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% 2i256 -> -28_948_022_309_329_048_855_892_746_252_171_976_963_317_496_166_410_141_009_864_396_001_978_282_409_984i256, 0i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% -2i256 -> 28_948_022_309_329_048_855_892_746_252_171_976_963_317_496_166_410_141_009_864_396_001_978_282_409_984i256, 0i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% 3i256 -> -19_298_681_539_552_699_237_261_830_834_781_317_975_544_997_444_273_427_339_909_597_334_652_188_273_322i256, -2i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% 7i256 -> -8_270_863_516_951_156_815_969_356_072_049_136_275_233_570_333_260_040_288_532_684_571_993_794_974_281i256, -1i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% 65_535i256 -> -883_437_012_568_216_948_375_455_748_902_784_068_461_661_590_490_887_037_761_940_825_573_457_920i256, -32_768i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 /,% 4_294_967_295i256 -> -13_479_973_336_713_870_765_757_598_744_263_302_691_024_331_148_677_648_317_163_446_992_896i256, -2_147_483_648i256
    4_294_967_296i256 /,% 4_294_967_296i256 -> 1i256, 0i256
    8_589_934_591i256 /,% 4_294_967_296i256 -> 1i256, 4_294_967_295i256
    12_884_901_887i256 /,% 4_294_967_296i256 -> 2i256, 4_294_967_295i256
    18_446_744_073_709_551_615i256 /,% 18_446_744_073_709_551_615i256 -> 1i256, 0i256
    340_282_366_920_938_463_463_374_607_431_768_211_455i256 /,% 340_282_366_920_938_463_463_374_607_431_768_211_455i256 -> 1i256, 0i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 -> 1i256, 0i256
    340_282_366_920_938_463_463_374_607_431_768_211_455i256 /,% 18_446_744_073_709_551_615i256 -> 18_446_744_073_709_551_617i256, 0i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 /,% 340_282_366_920_938_463_463_374_607_431_768_211_455i256 -> 170_141_183_460_469_231_731_687_303_715_884_105_728i256, 170_141_183_460_469_231_731_687_303_715_884_105_727i256
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
    (min_value, min_value);
    (max_value, min_value);
    (min_value, max_value);
    (max_value, max_value);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    bit_{and,or,xor} 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    bit_{and,or,xor} 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    bit_{and,or,xor} 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256, 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
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
    min_value;
    max_value;
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256
    bit_not 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
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
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0, 256, 256
    bit_{pop,clz,ctz} 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 1, 255, 0
    bit_{pop,clz,ctz} 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 1, 0, 255
    bit_{pop,clz,ctz} 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 256, 0, 0
    |}]

let%expect_test "**" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a ** %a -> %a\n" pp x pp y pp (x ** y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "-1");

    (max_value, of_string "0");
    (max_value, of_string "1");
    (max_value, max_value);
    (max_value, of_string "-254");
    (max_value, of_string "-255");
    (max_value, of_string "-256");

    (min_value, one);
    (min_value, neg_one);

    (of_string "1", max_value);
    (of_string "-1", max_value);

    (of_string "1", min_value);
    (of_string "-1", min_value);

    (of_string "2", of_string "31");
    (of_string "2", of_string "32");
    (of_string "2", of_string "63");
    (of_string "2", of_string "64");
    (of_string "2", of_string "127");
    (of_string "2", of_string "128");
    (of_string "2", of_string "255");
    (of_string "2", of_string "256");
    (of_string "2", of_string "257");

    (of_string "-2", of_string "31");
    (of_string "-2", of_string "32");
    (of_string "-2", of_string "63");
    (of_string "-2", of_string "64");
    (of_string "-2", of_string "127");
    (of_string "-2", of_string "128");
    (of_string "-2", of_string "255");
    (of_string "-2", of_string "256");
    (of_string "-2", of_string "257");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0i256 ** 0i256 -> 1i256
    0i256 ** 1i256 -> 0i256
    1i256 ** -1i256 -> 1i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 ** 0i256 -> 1i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 ** 1i256 -> 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 ** 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 -> 0i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 ** -254i256 -> 1i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 ** -255i256 -> 0i256
    57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 ** -256i256 -> 1i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 ** 1i256 -> -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256
    -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 ** -1i256 -> 0i256
    1i256 ** 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 -> 1i256
    -1i256 ** 57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_967i256 -> -1i256
    1i256 ** -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 -> 1i256
    -1i256 ** -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256 -> 1i256
    2i256 ** 31i256 -> 2_147_483_648i256
    2i256 ** 32i256 -> 4_294_967_296i256
    2i256 ** 63i256 -> 9_223_372_036_854_775_808i256
    2i256 ** 64i256 -> 18_446_744_073_709_551_616i256
    2i256 ** 127i256 -> 170_141_183_460_469_231_731_687_303_715_884_105_728i256
    2i256 ** 128i256 -> 340_282_366_920_938_463_463_374_607_431_768_211_456i256
    2i256 ** 255i256 -> -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256
    2i256 ** 256i256 -> 0i256
    2i256 ** 257i256 -> 0i256
    -2i256 ** 31i256 -> -2_147_483_648i256
    -2i256 ** 32i256 -> 4_294_967_296i256
    -2i256 ** 63i256 -> -9_223_372_036_854_775_808i256
    -2i256 ** 64i256 -> 18_446_744_073_709_551_616i256
    -2i256 ** 127i256 -> -170_141_183_460_469_231_731_687_303_715_884_105_728i256
    -2i256 ** 128i256 -> 340_282_366_920_938_463_463_374_607_431_768_211_456i256
    -2i256 ** 255i256 -> -57_896_044_618_658_097_711_785_492_504_343_953_926_634_992_332_820_282_019_728_792_003_956_564_819_968i256
    -2i256 ** 256i256 -> 0i256
    -2i256 ** 257i256 -> 0i256
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
    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> false
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> true
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256 -> true
    is_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0003i256 -> false
    is_pow2 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> false
    is_pow2 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> false
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
    max_value;
  ] in
  test us;
  printf "@]";

  [%expect{|
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0003i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0004i256
    floor_pow2,ceil_pow2 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0x4000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
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
    max_value;
  ] in
  test us;
  printf "@]";

  [%expect{|
        floor_lg,ceil_lg 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0i256, 0i256
        floor_lg,ceil_lg 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002i256 -> 1i256, 1i256
        floor_lg,ceil_lg 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0003i256 -> 1i256, 2i256
        floor_lg,ceil_lg 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 254i256, 255i256
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
    (of_string "0", of_string "-1");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256 -> 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001i256
    min,max 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi256, 0x0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000i256
   |}]
