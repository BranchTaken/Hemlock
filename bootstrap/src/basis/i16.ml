(* Partial Rudiments. *)
open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = sint
  let bit_length = 16
end
include T
include Intnb.MakeI(T)

let to_sint t =
  t

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
  narrow_of_signed (sint_of_int x)

let to_uns t =
  uns_of_sint t

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
    hash_fold 0x8000i16 -> 0x8bf5_6f8c_da2e_cc12_9596_673a_36eb_a422u128
    hash_fold 0xffffi16 -> 0x6921_12c9_6b4a_46af_a0e4_b27a_1aba_ed73u128
    hash_fold 0x0000i16 -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold 0x0001i16 -> 0x3d8a_cdb4_d36d_9c06_0044_03b7_fb05_c44au128
    hash_fold 0x7fffi16 -> 0x29f0_5d38_26f9_2b90_4dbf_5c8e_a2ee_799du128
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
    -32768i16 0x8000i16
    -1i16 0xffffi16
    0i16 0x0000i16
    1i16 0x0001i16
    32767i16 0x7fffi16
    |}]

let%expect_test "limits" =
  let open Format in

  printf "bit_length=%a\n" Uns.pp bit_length;
  printf "min_value=%a %a\n" pp min_value pp_x min_value;
  printf "max_value=%a %a\n" pp max_value pp_x max_value;

  [%expect{|
    bit_length=16
    min_value=-32768i16 0x8000i16
    max_value=32767i16 0x7fffi16
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
  fn (kv 0) (kv (-0x8000));
  printf "\n";
  fn (kv 0) (kv (-1));
  printf "\n";
  fn (kv (-0x8000)) (kv (-1));
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
    cmp 0x0000i16 0x8000i16 -> Gt
    0x0000i16 >= 0x8000i16 -> true
    0x0000i16 <= 0x8000i16 -> false
    0x0000i16 = 0x8000i16 -> false
    0x0000i16 > 0x8000i16 -> true
    0x0000i16 < 0x8000i16 -> false
    0x0000i16 <> 0x8000i16 -> true
    ascending 0x0000i16 0x8000i16 -> Gt
    descending 0x0000i16 0x8000i16 -> Lt

    cmp 0x0000i16 0xffffi16 -> Gt
    0x0000i16 >= 0xffffi16 -> true
    0x0000i16 <= 0xffffi16 -> false
    0x0000i16 = 0xffffi16 -> false
    0x0000i16 > 0xffffi16 -> true
    0x0000i16 < 0xffffi16 -> false
    0x0000i16 <> 0xffffi16 -> true
    ascending 0x0000i16 0xffffi16 -> Gt
    descending 0x0000i16 0xffffi16 -> Lt

    cmp 0x8000i16 0xffffi16 -> Lt
    0x8000i16 >= 0xffffi16 -> false
    0x8000i16 <= 0xffffi16 -> true
    0x8000i16 = 0xffffi16 -> false
    0x8000i16 > 0xffffi16 -> false
    0x8000i16 < 0xffffi16 -> true
    0x8000i16 <> 0xffffi16 -> true
    ascending 0x8000i16 0xffffi16 -> Lt
    descending 0x8000i16 0xffffi16 -> Gt

    clamp ~min:0xffffi16 ~max:0x0000i16 0xfffei16 -> 0xffffi16
    between ~low:0xffffi16 ~high:0x0000i16 0xfffei16 -> false

    clamp ~min:0xffffi16 ~max:0x0000i16 0xffffi16 -> 0xffffi16
    between ~low:0xffffi16 ~high:0x0000i16 0xffffi16 -> true

    clamp ~min:0xffffi16 ~max:0x0000i16 0x0000i16 -> 0x0000i16
    between ~low:0xffffi16 ~high:0x0000i16 0x0000i16 -> true

    clamp ~min:0xffffi16 ~max:0x0000i16 0x0001i16 -> 0x0000i16
    between ~low:0xffffi16 ~high:0x0000i16 0x0001i16 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);

  [%expect{|
    max_value + 1i16 -> 0x8000i16
    min_value - 1i16 -> 0x7fffi16
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
    of_sint 0x7fffffffffffffffi -> to_sint 0xffffi16 -> of_sint -1i -> 0xffffi16
    of_uns 0x7fffffffffffffffi -> to_uns 0x7fffi16 -> of_uns 0x0000000000007fff -> 0x7fffi16
    of_sint 0x7ffffffffffffffei -> to_sint 0xfffei16 -> of_sint -2i -> 0xfffei16
    of_uns 0x7ffffffffffffffei -> to_uns 0x7ffei16 -> of_uns 0x0000000000007ffe -> 0x7ffei16
    of_sint 0x7fffffffffffffffi -> to_sint 0xffffi16 -> of_sint -1i -> 0xffffi16
    of_uns 0x7fffffffffffffffi -> to_uns 0x7fffi16 -> of_uns 0x0000000000007fff -> 0x7fffi16
    of_sint 0x0000000000000000i -> to_sint 0x0000i16 -> of_sint 0i -> 0x0000i16
    of_uns 0x0000000000000000i -> to_uns 0x0000i16 -> of_uns 0x0000000000000000 -> 0x0000i16
    of_sint 0x0000000000000001i -> to_sint 0x0001i16 -> of_sint 1i -> 0x0001i16
    of_uns 0x0000000000000001i -> to_uns 0x0001i16 -> of_uns 0x0000000000000001 -> 0x0001i16
    of_sint 0x0000000000000002i -> to_sint 0x0002i16 -> of_sint 2i -> 0x0002i16
    of_uns 0x0000000000000002i -> to_uns 0x0002i16 -> of_uns 0x0000000000000002 -> 0x0002i16
    of_sint 0x3fffffffffffffffi -> to_sint 0x7fffi16 -> of_sint 32767i -> 0x7fffi16
    of_uns 0x3fffffffffffffffi -> to_uns 0x7fffi16 -> of_uns 0x0000000000007fff -> 0x7fffi16
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

    0x1p14;
    0x1p15;
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
    neg_one;
    zero;
    one;
    max_value;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_real -0x1p+0 -> 0xffffi16; to_real -> -0x1p+0
    of_real 0x0p+0 -> 0x0000i16; to_real -> 0x0p+0
    of_real 0x1.1p-1 -> 0x0000i16; to_real -> 0x0p+0
    of_real 0x1p+0 -> 0x0001i16; to_real -> 0x1p+0
    of_real 0x1.fffffffffffffp+48 -> 0x7fffi16; to_real -> 0x1.fffcp+14
    of_real 0x1.fffffffffffffp+52 -> 0x7fffi16; to_real -> 0x1.fffcp+14
    of_real 0x1.fffffffffffffp+56 -> 0x7ff0i16; to_real -> 0x1.ffcp+14
    of_real 0x1.fffffffffffffp+61 -> 0x7e00i16; to_real -> 0x1.f8p+14
    of_real 0x1p+14 -> 0x4000i16; to_real -> 0x1p+14
    of_real 0x1p+15 -> 0x0000i16; to_real -> 0x0p+0

    to_real 0xffffi16 -> -0x1p+0; of_real -> 0xffffi16
    to_real 0x0000i16 -> 0x0p+0; of_real -> 0x0000i16
    to_real 0x0001i16 -> 0x1p+0; of_real -> 0x0001i16
    to_real 0x7fffi16 -> 0x1.fffcp+14; of_real -> 0x7fffi16
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
    bit_{and,or,xor} 0x0000i16 0x0000i16 -> 0x0000i16, 0x0000i16, 0x0000i16
    bit_{and,or,xor} 0x7fffi16 0x0000i16 -> 0x0000i16, 0x7fffi16, 0x7fffi16
    bit_{and,or,xor} 0x0000i16 0x7fffi16 -> 0x0000i16, 0x7fffi16, 0x7fffi16
    bit_{and,or,xor} 0x7fffi16 0x7fffi16 -> 0x7fffi16, 0x7fffi16, 0x0000i16
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
    bit_not 0x0000i16 -> 0xffffi16
    bit_not 0x7fffi16 -> 0x8000i16
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
    kv (-0x8000);
    kv (-1);
    kv 0;
    kv 1;
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x8000i16 -> 1, 0, 15
    bit_{pop,clz,ctz} 0xffffi16 -> 16, 0, 0
    bit_{pop,clz,ctz} 0x0000i16 -> 0, 16, 16
    bit_{pop,clz,ctz} 0x0001i16 -> 1, 15, 0
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

    (kv 2, kv 7);
    (kv 2, kv 8);
    (kv 2, kv 15);
    (kv 2, kv 16);

    (kv 0xf, kv 0xf);
    (kv 0xff, kv 0xff);

    (kv 1, kv (-1));

    (kv (-1), kv (-1));

  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000i16 ** 0x0000i16 -> 0x0001i16
    0x0000i16 ** 0x0001i16 -> 0x0000i16
    0x0001i16 ** 0x0000i16 -> 0x0001i16
    0x0001i16 ** 0x0001i16 -> 0x0001i16
    0xffffi16 ** 0x0000i16 -> 0x0001i16
    0xffffi16 ** 0x0001i16 -> 0xffffi16
    0x0002i16 ** 0x0007i16 -> 0x0080i16
    0x0002i16 ** 0x0008i16 -> 0x0100i16
    0x0002i16 ** 0x000fi16 -> 0x8000i16
    0x0002i16 ** 0x0010i16 -> 0x0000i16
    0x000fi16 ** 0x000fi16 -> 0x07efi16
    0x00ffi16 ** 0x00ffi16 -> 0xfeffi16
    0x0001i16 ** 0xffffi16 -> 0x0001i16
    0xffffi16 ** 0xffffi16 -> 0xffffi16
    |}]
