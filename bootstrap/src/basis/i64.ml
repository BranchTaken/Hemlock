include Rudiments_int
open Rudiments

module T = struct
  type t = u64

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u128 1 ~f:(fun _ -> {hi=Int64.zero; lo=t})
    |> Hash.State.Gen.fini

  let cmp t0 t1 =
    Isize.cmp (Usize.to_isize Int64.(compare t0 t1)) (Isize.kv 0)

  let zero = Int64.zero

  let one = Int64.one

  let pp ppf t =
    Format.fprintf ppf "%Ldi64" t
end
include T
include Identifiable.Make(T)
include Cmpable.Make_zero(T)

let pp_x ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Usize.(shift < 64) then Format.fprintf ppf "_";
        let shift' = shift - 16 in
        Format.fprintf ppf "%04Lx"
          Int64.(logand (shift_right_logical x shift') (of_int 0xffff));
        fn x shift'
      end
  end in
  Format.fprintf ppf "0x";
  fn t 64;
  Format.fprintf ppf "i64"

let of_string s =
  Int64.of_string s

let to_string t =
  Format.asprintf "%a" pp t

let of_float f =
  (* OCaml handles overflow poorly, but this deficiency has no anticipated
   * impact on bootstrapping. *)
  Int64.of_float f

let to_float t =
  Int64.to_float t

let of_usize u =
  let i = Usize.to_isize u in
  match Isize.(i >= (kv 0)) with
  | true -> Int64.of_int u
  | false -> begin
      let isize_sign_bit = Usize.of_isize Isize.min_value in
      Int64.(add (of_int u)
        (add (of_int isize_sign_bit)
            (of_int isize_sign_bit)))
    end

let min_value = Int64.min_int

let max_value = Int64.max_int

let succ = U64.succ

let pred = U64.pred

let bit_and = U64.bit_and

let bit_or = U64.bit_or

let bit_xor = U64.bit_xor

let bit_not = U64.bit_not

let bit_sl = U64.bit_sl

let bit_usr = U64.bit_usr

let bit_ssr = U64.bit_ssr

let ( + ) = U64.( + )

let ( - ) = U64.( - )

let ( * ) = U64.( * )

let ( / ) = U64.( / )

let ( % ) = U64.( % )

let ( ** ) t0 t1 =
  (* Decompose the exponent to limit algorithmic complexity. *)
  let rec fn r p n = begin
    match n = zero with
    | true -> r
    | false -> begin
        let r' =
          match (bit_and n one) = zero with
          | true -> r
          | false -> r * p
        in
        let p' = p * p in
        let n' = bit_usr ~shift:1 n in
        fn r' p' n'
      end
  end in
  fn one t0 t1

let ( // ) t0 t1 =
  (to_float t0) /. (to_float t1)

let bit_pop = U64.bit_pop

let bit_clz = U64.bit_clz

let bit_ctz = U64.bit_ctz

module U = struct
  type nonrec t = t

  let num_bits = 64

  let cmp = cmp
  let zero = zero
  let one = one
  let of_usize = of_usize
  let ( + ) = ( + )
  let ( - ) = ( - )
  let bit_and = bit_and
  let bit_sl = bit_sl
  let bit_clz = bit_clz
end
include Intnb.Make_derived(U)

let abs t =
  Int64.abs t

let neg t =
  Int64.neg t

let ( ~+ ) t =
  t

let ( ~- ) t =
  neg t

let neg_one = Int64.minus_one

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
    hash_fold 0x0000_0000_0000_0000i64 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x0000_0000_0000_0001i64 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x8000_0000_0000_0000i64 -> 0xeec2_2c93_f68d_d3ec_fae5_b54a_bd13_1d53u128
    hash_fold 0x7fff_ffff_ffff_ffffi64 -> 0x6742_c7ab_b64b_04c2_105e_972f_0c7b_e315u128
    |}]

let%expect_test "limits" =
  let open Format in

  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    min_value=0x8000_0000_0000_0000i64
    max_value=0x7fff_ffff_ffff_ffffi64
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
  fn zero (of_string "0x8000_0000_0000_0000");
  printf "\n";
  fn zero (of_string "0xffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp min pp max pp t pp (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp t pp min pp max (between ~low:min ~high:max t);
  end in
  fn2 (of_string "-2") (of_string "-1") (of_string "1");
  fn2 (of_string "-1") (of_string "-1") (of_string "1");
  fn2 (of_string "0") (of_string "-1") (of_string "1");
  fn2 (of_string "1") (of_string "-1") (of_string "1");
  fn2 (of_string "2") (of_string "-1") (of_string "1");

  [%expect{|
    cmp 0x0000_0000_0000_0000i64 0x8000_0000_0000_0000i64 -> Gt
    0x0000_0000_0000_0000i64 >= 0x8000_0000_0000_0000i64 -> true
    0x0000_0000_0000_0000i64 <= 0x8000_0000_0000_0000i64 -> false
    0x0000_0000_0000_0000i64 = 0x8000_0000_0000_0000i64 -> false
    0x0000_0000_0000_0000i64 > 0x8000_0000_0000_0000i64 -> true
    0x0000_0000_0000_0000i64 < 0x8000_0000_0000_0000i64 -> false
    0x0000_0000_0000_0000i64 <> 0x8000_0000_0000_0000i64 -> true
    ascending 0x0000_0000_0000_0000i64 0x8000_0000_0000_0000i64 -> Gt
    descending 0x0000_0000_0000_0000i64 0x8000_0000_0000_0000i64 -> Lt

    cmp 0x0000_0000_0000_0000i64 0xffff_ffff_ffff_ffffi64 -> Gt
    0x0000_0000_0000_0000i64 >= 0xffff_ffff_ffff_ffffi64 -> true
    0x0000_0000_0000_0000i64 <= 0xffff_ffff_ffff_ffffi64 -> false
    0x0000_0000_0000_0000i64 = 0xffff_ffff_ffff_ffffi64 -> false
    0x0000_0000_0000_0000i64 > 0xffff_ffff_ffff_ffffi64 -> true
    0x0000_0000_0000_0000i64 < 0xffff_ffff_ffff_ffffi64 -> false
    0x0000_0000_0000_0000i64 <> 0xffff_ffff_ffff_ffffi64 -> true
    ascending 0x0000_0000_0000_0000i64 0xffff_ffff_ffff_ffffi64 -> Gt
    descending 0x0000_0000_0000_0000i64 0xffff_ffff_ffff_ffffi64 -> Lt

    cmp 0x8000_0000_0000_0000i64 0x7fff_ffff_ffff_ffffi64 -> Lt
    0x8000_0000_0000_0000i64 >= 0x7fff_ffff_ffff_ffffi64 -> false
    0x8000_0000_0000_0000i64 <= 0x7fff_ffff_ffff_ffffi64 -> true
    0x8000_0000_0000_0000i64 = 0x7fff_ffff_ffff_ffffi64 -> false
    0x8000_0000_0000_0000i64 > 0x7fff_ffff_ffff_ffffi64 -> false
    0x8000_0000_0000_0000i64 < 0x7fff_ffff_ffff_ffffi64 -> true
    0x8000_0000_0000_0000i64 <> 0x7fff_ffff_ffff_ffffi64 -> true
    ascending 0x8000_0000_0000_0000i64 0x7fff_ffff_ffff_ffffi64 -> Lt
    descending 0x8000_0000_0000_0000i64 0x7fff_ffff_ffff_ffffi64 -> Gt

    clamp ~min:-1i64 ~max:1i64 -2i64 -> -1i64
    between ~low:-2i64 ~high:-1i64 1i64 -> false

    clamp ~min:-1i64 ~max:1i64 -1i64 -> -1i64
    between ~low:-1i64 ~high:-1i64 1i64 -> true

    clamp ~min:-1i64 ~max:1i64 0i64 -> 0i64
    between ~low:0i64 ~high:-1i64 1i64 -> true

    clamp ~min:-1i64 ~max:1i64 1i64 -> 1i64
    between ~low:1i64 ~high:-1i64 1i64 -> true

    clamp ~min:-1i64 ~max:1i64 2i64 -> 1i64
    between ~low:2i64 ~high:-1i64 1i64 -> false
    |}]

let%expect_test "narrowing" =
  let open Format in
  let fifteen = of_string "15" in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1i64 -> 0x8000_0000_0000_0000i64
    min_value - 1i64 -> 0x7fff_ffff_ffff_ffffi64
    max_value * 15i64 -> 0x7fff_ffff_ffff_fff1i64
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
    0x1.f_ffff_ffff_ffffp62;
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
    min_value;
    max_value;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_float -0x1p+0 -> 0xffff_ffff_ffff_ffffi64; to_float -> -0x1p+0
    of_float 0x0p+0 -> 0x0000_0000_0000_0000i64; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x0000_0000_0000_0000i64; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x0000_0000_0000_0001i64; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0x0001_ffff_ffff_ffffi64; to_float -> 0x1.ffffffffffffp+48
    of_float 0x1.fffffffffffffp+52 -> 0x001f_ffff_ffff_ffffi64; to_float -> 0x1.fffffffffffffp+52
    of_float 0x1.fffffffffffffp+56 -> 0x01ff_ffff_ffff_fff0i64; to_float -> 0x1.fffffffffffffp+56
    of_float 0x1.fffffffffffffp+62 -> 0x7fff_ffff_ffff_fc00i64; to_float -> 0x1.fffffffffffffp+62

    to_float 0x0000_0000_0000_0000i64 -> 0x0p+0; of_float -> 0x0000_0000_0000_0000i64
    to_float 0x0000_0000_0000_0001i64 -> 0x1p+0; of_float -> 0x0000_0000_0000_0001i64
    to_float 0x8000_0000_0000_0000i64 -> -0x1p+63; of_float -> 0x8000_0000_0000_0000i64
    to_float 0x7fff_ffff_ffff_ffffi64 -> 0x1p+63; of_float -> 0x8000_0000_0000_0000i64
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
    (of_string "0xffff_ffff_ffff_ffff", of_string "0");
    (of_string "0", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0000_0000_0000_0000i64 0x0000_0000_0000_0000i64 -> 0x0000_0000_0000_0000i64, 0x0000_0000_0000_0000i64, 0x0000_0000_0000_0000i64
    bit_{and,or,xor} 0xffff_ffff_ffff_ffffi64 0x0000_0000_0000_0000i64 -> 0x0000_0000_0000_0000i64, 0xffff_ffff_ffff_ffffi64, 0xffff_ffff_ffff_ffffi64
    bit_{and,or,xor} 0x0000_0000_0000_0000i64 0xffff_ffff_ffff_ffffi64 -> 0x0000_0000_0000_0000i64, 0xffff_ffff_ffff_ffffi64, 0xffff_ffff_ffff_ffffi64
    bit_{and,or,xor} 0xffff_ffff_ffff_ffffi64 0xffff_ffff_ffff_ffffi64 -> 0xffff_ffff_ffff_ffffi64, 0xffff_ffff_ffff_ffffi64, 0x0000_0000_0000_0000i64
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
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0000_0000_0000_0000i64 -> 0xffff_ffff_ffff_ffffi64
    bit_not 0xffff_ffff_ffff_ffffi64 -> 0x0000_0000_0000_0000i64
    |}]

let%expect_test "bit_pop,bit_clz,bit_ctz" =
  let open Format in
  printf "@[<h>";
  let rec test_u64s = function
    | [] -> ()
    | u :: us' -> begin
        printf "bit_{pop,clz,ctz} %a -> %u, %u, %u\n"
          pp_x u (bit_pop u) (bit_clz u) (bit_ctz u);
        test_u64s us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test_u64s us;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x0000_0000_0000_0000i64 -> 0, 64, 64
    bit_{pop,clz,ctz} 0x0000_0000_0000_0001i64 -> 1, 63, 0
    bit_{pop,clz,ctz} 0x8000_0000_0000_0000i64 -> 1, 0, 63
    bit_{pop,clz,ctz} 0xffff_ffff_ffff_ffffi64 -> 64, 0, 0
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

    (of_string "0x7fff_ffff_ffff_ffff", of_string "0");
    (of_string "0x7fff_ffff_ffff_ffff", of_string "1");

    (of_string "2", of_string "31");
    (of_string "2", of_string "32");
    (of_string "2", of_string "63");
    (of_string "2", of_string "64");

    (of_string "0xf", of_string "0xf");
    (of_string "0xff", of_string "0xff");

    (of_string "1", of_string "0x7fff_ffff_ffff_ffff");

    (of_string "0x7fff_ffff_ffff_ffff", of_string "0x7fff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000i64 ** 0x0000_0000_0000_0000i64 -> 0x0000_0000_0000_0001i64
    0x0000_0000_0000_0000i64 ** 0x0000_0000_0000_0001i64 -> 0x0000_0000_0000_0000i64
    0x7fff_ffff_ffff_ffffi64 ** 0x0000_0000_0000_0000i64 -> 0x0000_0000_0000_0001i64
    0x7fff_ffff_ffff_ffffi64 ** 0x0000_0000_0000_0001i64 -> 0x7fff_ffff_ffff_ffffi64
    0x0000_0000_0000_0002i64 ** 0x0000_0000_0000_001fi64 -> 0x0000_0000_8000_0000i64
    0x0000_0000_0000_0002i64 ** 0x0000_0000_0000_0020i64 -> 0x0000_0001_0000_0000i64
    0x0000_0000_0000_0002i64 ** 0x0000_0000_0000_003fi64 -> 0x8000_0000_0000_0000i64
    0x0000_0000_0000_0002i64 ** 0x0000_0000_0000_0040i64 -> 0x0000_0000_0000_0000i64
    0x0000_0000_0000_000fi64 ** 0x0000_0000_0000_000fi64 -> 0x0613_b62c_5977_07efi64
    0x0000_0000_0000_00ffi64 ** 0x0000_0000_0000_00ffi64 -> 0x5997_756b_007f_feffi64
    0x0000_0000_0000_0001i64 ** 0x7fff_ffff_ffff_ffffi64 -> 0x0000_0000_0000_0001i64
    0x7fff_ffff_ffff_ffffi64 ** 0x7fff_ffff_ffff_ffffi64 -> 0x7fff_ffff_ffff_ffffi64
    |}]
