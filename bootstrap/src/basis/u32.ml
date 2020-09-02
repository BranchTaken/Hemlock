(* Partial Rudiments. *)
open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = uns
  let bit_length = 32
end
include T
include Intnb.MakeU(T)

let to_sint t =
  Uns.to_sint t

let of_sint x =
  narrow_of_signed x

let of_sint_opt x =
  let t = of_sint x in
  let x' = to_sint t in
  match Sint.(x' = x) with
  | false -> None
  | true -> Some t

let of_sint_hlt x =
  match of_sint_opt x with
  | None -> halt "Lossy conversion"
  | Some t -> t

let kv x =
  narrow_of_unsigned x

let to_uns t =
  t

let of_uns x =
  narrow_of_unsigned x

let of_uns_opt x =
  let t = of_uns x in
  let x' = to_uns t in
  match Uns.(x' = x) with
  | false -> None
  | true -> Some t

let of_uns_hlt x =
  match of_uns_opt x with
  | None -> halt "Lossy conversion"
  | Some t -> t

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
    hash_fold 0x00000000u32 -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold 0x00000001u32 -> 0x3d8a_cdb4_d36d_9c06_0044_03b7_fb05_c44au128
    hash_fold 0x00000000u32 -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold 0xffffffffu32 -> 0x9670_07f2_6798_4cd6_ab87_9e40_456e_a4f6u128
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
  fn [kv 0; kv 1; kv 42; kv 0x1_ffff_ffff];
  printf "@]";

  [%expect{|
    0u32 0x00000000u32
    1u32 0x00000001u32
    42u32 0x0000002au32
    4294967295u32 0xffffffffu32
    |}]

let%expect_test "limits" =
  let open Format in

  printf "bit_length=%a\n" Uns.pp bit_length;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    bit_length=32
    min_value=0x00000000u32
    max_value=0xffffffffu32
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
  fn (kv 0) (kv 0x8000_0000);
  printf "\n";
  fn (kv 0) (kv 0xffff_ffff);
  printf "\n";
  fn (kv 0x8000_0000) (kv 0xffff_ffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv 0x7fff_fffe) (kv 0x7fff_ffff) (kv 0x8000_0001);
  fn2 (kv 0x7fff_ffff) (kv 0x7fff_ffff) (kv 0x8000_0001);
  fn2 (kv 0x8000_0000) (kv 0x7fff_ffff) (kv 0x8000_0001);
  fn2 (kv 0x8000_0001) (kv 0x7fff_ffff) (kv 0x8000_0001);
  fn2 (kv 0x8000_0002) (kv 0x7fff_ffff) (kv 0x8000_0001);

  [%expect{|
    cmp 0x00000000u32 0x80000000u32 -> Lt
    0x00000000u32 >= 0x80000000u32 -> false
    0x00000000u32 <= 0x80000000u32 -> true
    0x00000000u32 = 0x80000000u32 -> false
    0x00000000u32 > 0x80000000u32 -> false
    0x00000000u32 < 0x80000000u32 -> true
    0x00000000u32 <> 0x80000000u32 -> true
    ascending 0x00000000u32 0x80000000u32 -> Lt
    descending 0x00000000u32 0x80000000u32 -> Gt

    cmp 0x00000000u32 0xffffffffu32 -> Lt
    0x00000000u32 >= 0xffffffffu32 -> false
    0x00000000u32 <= 0xffffffffu32 -> true
    0x00000000u32 = 0xffffffffu32 -> false
    0x00000000u32 > 0xffffffffu32 -> false
    0x00000000u32 < 0xffffffffu32 -> true
    0x00000000u32 <> 0xffffffffu32 -> true
    ascending 0x00000000u32 0xffffffffu32 -> Lt
    descending 0x00000000u32 0xffffffffu32 -> Gt

    cmp 0x80000000u32 0xffffffffu32 -> Lt
    0x80000000u32 >= 0xffffffffu32 -> false
    0x80000000u32 <= 0xffffffffu32 -> true
    0x80000000u32 = 0xffffffffu32 -> false
    0x80000000u32 > 0xffffffffu32 -> false
    0x80000000u32 < 0xffffffffu32 -> true
    0x80000000u32 <> 0xffffffffu32 -> true
    ascending 0x80000000u32 0xffffffffu32 -> Lt
    descending 0x80000000u32 0xffffffffu32 -> Gt

    clamp ~min:0x7fffffffu32 ~max:0x80000001u32 0x7ffffffeu32 -> 0x7fffffffu32
    between ~low:0x7fffffffu32 ~high:0x80000001u32 0x7ffffffeu32 -> false

    clamp ~min:0x7fffffffu32 ~max:0x80000001u32 0x7fffffffu32 -> 0x7fffffffu32
    between ~low:0x7fffffffu32 ~high:0x80000001u32 0x7fffffffu32 -> true

    clamp ~min:0x7fffffffu32 ~max:0x80000001u32 0x80000000u32 -> 0x80000000u32
    between ~low:0x7fffffffu32 ~high:0x80000001u32 0x80000000u32 -> true

    clamp ~min:0x7fffffffu32 ~max:0x80000001u32 0x80000001u32 -> 0x80000001u32
    between ~low:0x7fffffffu32 ~high:0x80000001u32 0x80000001u32 -> true

    clamp ~min:0x7fffffffu32 ~max:0x80000001u32 0x80000002u32 -> 0x80000001u32
    between ~low:0x7fffffffu32 ~high:0x80000001u32 0x80000002u32 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  let fifteen = (kv 15) in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1u32 -> 0x00000000u32
    min_value - 1u32 -> 0xffffffffu32
    max_value * 15u32 -> 0xfffffff1u32
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
    of_sint 0x7fffffffffffffffi -> to_sint 0xffffffffu32 -> of_sint 0x00000000ffffffffi -> 0xffffffffu32
    of_uns 0x7fffffffffffffff -> to_uns 0xffffffffu32 -> of_uns 0x00000000ffffffff -> 0xffffffffu32
    of_sint 0x0000000000000000i -> to_sint 0x00000000u32 -> of_sint 0x0000000000000000i -> 0x00000000u32
    of_uns 0x0000000000000000 -> to_uns 0x00000000u32 -> of_uns 0x0000000000000000 -> 0x00000000u32
    of_sint 0x000000000000002ai -> to_sint 0x0000002au32 -> of_sint 0x000000000000002ai -> 0x0000002au32
    of_uns 0x000000000000002a -> to_uns 0x0000002au32 -> of_uns 0x000000000000002a -> 0x0000002au32
    of_sint 0x00000000001fffffi -> to_sint 0x001fffffu32 -> of_sint 0x00000000001fffffi -> 0x001fffffu32
    of_uns 0x00000000001fffff -> to_uns 0x001fffffu32 -> of_uns 0x00000000001fffff -> 0x001fffffu32
    of_sint 0x0000000000200000i -> to_sint 0x00200000u32 -> of_sint 0x0000000000200000i -> 0x00200000u32
    of_uns 0x0000000000200000 -> to_uns 0x00200000u32 -> of_uns 0x0000000000200000 -> 0x00200000u32
    of_sint 0x0000000000200001i -> to_sint 0x00200001u32 -> of_sint 0x0000000000200001i -> 0x00200001u32
    of_uns 0x0000000000200001 -> to_uns 0x00200001u32 -> of_uns 0x0000000000200001 -> 0x00200001u32
    of_sint 0x3fffffffffffffffi -> to_sint 0xffffffffu32 -> of_sint 0x00000000ffffffffi -> 0xffffffffu32
    of_uns 0x3fffffffffffffff -> to_uns 0xffffffffu32 -> of_uns 0x00000000ffffffff -> 0xffffffffu32
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

    0x1p31;
    0x1p32;
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
    of_real -0x1p+0 -> 0x00000000u32; to_real -> 0x0p+0
    of_real 0x0p+0 -> 0x00000000u32; to_real -> 0x0p+0
    of_real 0x1.1p-1 -> 0x00000000u32; to_real -> 0x0p+0
    of_real 0x1p+0 -> 0x00000001u32; to_real -> 0x1p+0
    of_real 0x1.fffffffffffffp+48 -> 0xffffffffu32; to_real -> 0x1.fffffffep+31
    of_real 0x1.fffffffffffffp+52 -> 0xffffffffu32; to_real -> 0x1.fffffffep+31
    of_real 0x1.fffffffffffffp+56 -> 0xfffffff0u32; to_real -> 0x1.ffffffep+31
    of_real 0x1.fffffffffffffp+61 -> 0xfffffe00u32; to_real -> 0x1.fffffcp+31
    of_real 0x1p+31 -> 0x80000000u32; to_real -> 0x1p+31
    of_real 0x1p+32 -> 0x00000000u32; to_real -> 0x0p+0

    to_real 0x00000000u32 -> 0x0p+0; of_real -> 0x00000000u32
    to_real 0x00000001u32 -> 0x1p+0; of_real -> 0x00000001u32
    to_real 0xffffffffu32 -> 0x1.fffffffep+31; of_real -> 0xffffffffu32
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
    bit_{and,or,xor} 0x00000000u32 0x00000000u32 -> 0x00000000u32, 0x00000000u32, 0x00000000u32
    bit_{and,or,xor} 0xffffffffu32 0x00000000u32 -> 0x00000000u32, 0xffffffffu32, 0xffffffffu32
    bit_{and,or,xor} 0x00000000u32 0xffffffffu32 -> 0x00000000u32, 0xffffffffu32, 0xffffffffu32
    bit_{and,or,xor} 0xffffffffu32 0xffffffffu32 -> 0xffffffffu32, 0xffffffffu32, 0x00000000u32
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
    bit_not 0x00000000u32 -> 0xffffffffu32
    bit_not 0xffffffffu32 -> 0x00000000u32
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
    kv 0x8000_0000;
    kv 0xffff_ffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x00000000u32 -> 0, 32, 32
    bit_{pop,clz,ctz} 0x00000001u32 -> 1, 31, 0
    bit_{pop,clz,ctz} 0x80000000u32 -> 1, 0, 31
    bit_{pop,clz,ctz} 0xffffffffu32 -> 32, 0, 0
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

    (0xffff_ffff, 0);
    (0xffff_ffff, 1);

    (2, 15);
    (2, 16);
    (2, 31);
    (2, 32);

    (0xf, 0xf);
    (0xff, 0xff);

    (1, 0xffff_ffff);

    (0xffff_ffff, 0xffff_ffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x00000000u32 ** 0x00000000u32 -> 0x00000001u32
    0x00000000u32 ** 0x00000001u32 -> 0x00000000u32
    0xffffffffu32 ** 0x00000000u32 -> 0x00000001u32
    0xffffffffu32 ** 0x00000001u32 -> 0xffffffffu32
    0x00000002u32 ** 0x0000000fu32 -> 0x00008000u32
    0x00000002u32 ** 0x00000010u32 -> 0x00010000u32
    0x00000002u32 ** 0x0000001fu32 -> 0x80000000u32
    0x00000002u32 ** 0x00000020u32 -> 0x00000000u32
    0x0000000fu32 ** 0x0000000fu32 -> 0x597707efu32
    0x000000ffu32 ** 0x000000ffu32 -> 0x007ffeffu32
    0x00000001u32 ** 0xffffffffu32 -> 0x00000001u32
    0xffffffffu32 ** 0xffffffffu32 -> 0xffffffffu32
    |}]
