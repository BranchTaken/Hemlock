(* Partial Rudiments. *)
include Rudiments_int0
module Usize = U63

let of_int t =
  isize_of_int t

let kv t =
  isize_of_int t

module T = struct
  type t = isize
  let num_bits = Sys.int_size
end
include T
include Intnb.Make_i(T)

(*******************************************************************************
 * Begin tests.
 *)

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
  fn [kv (-1); kv 0; kv 1; kv 42; kv 0x3fff_ffff_ffff_ffff];
  printf "@]";

  [%expect{|
    -1i 0x7fffffffffffffffi
    0i 0x0000000000000000i
    1i 0x0000000000000001i
    42i 0x000000000000002ai
    4611686018427387903i 0x3fffffffffffffffi
    |}]

let%expect_test "float" =
  let open Format in
  let x = of_float 0. in
  printf "%a\n" pp x;
  let f = to_float (kv 1) in
  printf "%.1f\n" f;

  [%expect{|
    0i
    1.0
    |}]

let%expect_test "string" =
  let open Format in
  let x = of_string "0" in
  printf "%a\n" pp x;
  let s = to_string (kv 1) in
  printf "%s\n" s;

  [%expect{|
    0i
    1i
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Usize.pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=63
    min_value=0x4000000000000000i
    max_value=0x3fffffffffffffffi
    |}]

let%expect_test "constants" =
  let open Format in
  printf "zero=%a\n" pp zero;
  printf "one=%a\n" pp one;
  printf "neg_one=%a\n" pp neg_one;

  [%expect{|
    zero=0i
    one=1i
    neg_one=-1i
    |}]

let%expect_test "is_" =
  let open Format in
  let ns = [kv (-1); kv 0; kv 1] in
  let rec fn ns = begin
    match ns with
    | [] -> ()
    | n :: ns' -> begin
        printf "%a\n" pp n;
        printf "  is_positive    =%b\n" (is_positive n);
        printf "  is_non_negative=%b\n" (is_non_negative n);
        printf "  is_negative    =%b\n" (is_negative n);
        printf "  is_non_positive=%b\n" (is_non_positive n);
        printf "\n";
        fn ns'
      end
  end in
  fn ns;

  [%expect{|
    -1i
      is_positive    =false
      is_non_negative=false
      is_negative    =true
      is_non_positive=true

    0i
      is_positive    =false
      is_non_negative=true
      is_negative    =false
      is_non_positive=true

    1i
      is_positive    =true
      is_non_negative=true
      is_negative    =false
      is_non_positive=false
    |}]

let%expect_test "seq" =
  let open Format in
  let x = kv 1 in
  printf "succ: %a -> %a\n" pp x pp (succ x);
  printf "pred: %a -> %a\n" pp x pp (pred x);

  [%expect{|
    succ: 1i -> 2i
    pred: 1i -> 0i
    |}]

let%expect_test "bit_" =
  let open Format in

  let x = kv 0b0011 in
  let y = kv 0b0101 in
  printf "bit_and %a %a -> %a\n" pp_x x pp_x y pp_x (bit_and x y);
  printf "bit_or %a %a -> %a\n" pp_x x pp_x y pp_x (bit_or x y);
  printf "bit_xor %a %a -> %a\n" pp_x x pp_x y pp_x (bit_xor x y);

  let x = kv 0b10 in
  printf "bit_not %a -> %a\n" pp_x x pp_x (bit_not x);

  let x = kv 0xff in
  let s = 4 in
  printf "bit_sl %a %a -> %a\n" pp_x x Usize.pp s pp_x (bit_sl x s);

  let x = kv (-1) in
  let s = 4 in
  printf "bit_usr %a %a -> %a\n" pp_x x Usize.pp s pp_x (bit_usr x s);
  printf "bit_ssr %a %a -> %a\n" pp_x x Usize.pp s pp_x (bit_ssr x s);

  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_pop %a -> %a\n" pp_x x Usize.pp (bit_pop x);
        printf "bit_clz %a -> %a\n" pp_x x Usize.pp (bit_clz x);
        printf "bit_ctz %a -> %a\n" pp_x x Usize.pp (bit_ctz x);
        fn xs'
      end
  end in
  fn [kv (-1); kv 0; kv 1; min_value; max_value; kv 0xf73100];

  [%expect{|
    bit_and 0x0000000000000003i 0x0000000000000005i -> 0x0000000000000001i
    bit_or 0x0000000000000003i 0x0000000000000005i -> 0x0000000000000007i
    bit_xor 0x0000000000000003i 0x0000000000000005i -> 0x0000000000000006i
    bit_not 0x0000000000000002i -> 0x7ffffffffffffffdi
    bit_sl 0x00000000000000ffi 4 -> 0x0000000000000ff0i
    bit_usr 0x7fffffffffffffffi 4 -> 0x07ffffffffffffffi
    bit_ssr 0x7fffffffffffffffi 4 -> 0x7fffffffffffffffi
    bit_pop 0x7fffffffffffffffi -> 63
    bit_clz 0x7fffffffffffffffi -> 0
    bit_ctz 0x7fffffffffffffffi -> 0
    bit_pop 0x0000000000000000i -> 0
    bit_clz 0x0000000000000000i -> 63
    bit_ctz 0x0000000000000000i -> 63
    bit_pop 0x0000000000000001i -> 1
    bit_clz 0x0000000000000001i -> 62
    bit_ctz 0x0000000000000001i -> 0
    bit_pop 0x4000000000000000i -> 1
    bit_clz 0x4000000000000000i -> 0
    bit_ctz 0x4000000000000000i -> 62
    bit_pop 0x3fffffffffffffffi -> 62
    bit_clz 0x3fffffffffffffffi -> 1
    bit_ctz 0x3fffffffffffffffi -> 0
    bit_pop 0x0000000000f73100i -> 10
    bit_clz 0x0000000000f73100i -> 39
    bit_ctz 0x0000000000f73100i -> 8
    |}]

let%expect_test "pow2_lg" =
  let open Format in
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "is_pow2 %a -> %b\n" pp_x x (is_pow2 x);
        printf "floor_pow2 %a -> %a\n" pp_x x pp_x (floor_pow2 x);
        printf "ceil_pow2 %a -> %a\n" pp_x x pp_x (ceil_pow2 x);
        if x > kv 0 then begin
          printf "floor_lg %a -> %a\n" pp_x x pp (floor_lg x);
          printf "ceil_lg %a -> %a\n" pp_x x pp (ceil_lg x)
        end;
        fn xs'
      end
  end in
  fn [kv 0; kv 1; kv 2; kv 3; kv 4; kv 0xf0; max_value];

  [%expect{|
    is_pow2 0x0000000000000000i -> true
    floor_pow2 0x0000000000000000i -> 0x0000000000000000i
    ceil_pow2 0x0000000000000000i -> 0x0000000000000000i
    is_pow2 0x0000000000000001i -> true
    floor_pow2 0x0000000000000001i -> 0x0000000000000001i
    ceil_pow2 0x0000000000000001i -> 0x0000000000000001i
    floor_lg 0x0000000000000001i -> 0i
    ceil_lg 0x0000000000000001i -> 0i
    is_pow2 0x0000000000000002i -> true
    floor_pow2 0x0000000000000002i -> 0x0000000000000002i
    ceil_pow2 0x0000000000000002i -> 0x0000000000000002i
    floor_lg 0x0000000000000002i -> 1i
    ceil_lg 0x0000000000000002i -> 1i
    is_pow2 0x0000000000000003i -> false
    floor_pow2 0x0000000000000003i -> 0x0000000000000002i
    ceil_pow2 0x0000000000000003i -> 0x0000000000000004i
    floor_lg 0x0000000000000003i -> 1i
    ceil_lg 0x0000000000000003i -> 2i
    is_pow2 0x0000000000000004i -> true
    floor_pow2 0x0000000000000004i -> 0x0000000000000004i
    ceil_pow2 0x0000000000000004i -> 0x0000000000000004i
    floor_lg 0x0000000000000004i -> 2i
    ceil_lg 0x0000000000000004i -> 2i
    is_pow2 0x00000000000000f0i -> false
    floor_pow2 0x00000000000000f0i -> 0x0000000000000080i
    ceil_pow2 0x00000000000000f0i -> 0x0000000000000100i
    floor_lg 0x00000000000000f0i -> 7i
    ceil_lg 0x00000000000000f0i -> 8i
    is_pow2 0x3fffffffffffffffi -> false
    floor_pow2 0x3fffffffffffffffi -> 0x2000000000000000i
    ceil_pow2 0x3fffffffffffffffi -> 0x4000000000000000i
    floor_lg 0x3fffffffffffffffi -> 61i
    ceil_lg 0x3fffffffffffffffi -> 62i
    |}]

let%expect_test "ops" =
  let open Format in
  let x = kv 4 in
  let y = kv 3 in
  printf "%a + %a -> %a\n" pp x pp y pp (x + y);
  printf "%a - %a -> %a\n" pp x pp y pp (x - y);
  printf "%a * %a -> %a\n" pp x pp y pp (x * y);
  printf "%a / %a -> %a\n" pp x pp y pp (x / y);
  printf "%a %% %a -> %a\n" pp x pp y pp (x % y);
  printf "%a ** %a -> %a\n" pp x pp y pp (x ** y);
  printf "%a // %a -> %.2f\n" pp x pp y (x // y);
  let z = kv (-2) in
  printf "-(%a) -> %a\n" pp x pp (-x);
  printf "~-(%a) -> %a\n" pp x pp ~-x;
  printf "+(%a) -> %a\n" pp z pp (+z);
  printf "~+(%a) -> %a\n" pp z pp ~+z;
  printf "neg %a -> %a\n" pp x pp (neg x);
  printf "neg %a -> %a\n" pp z pp (neg z);
  printf "abs %a -> %a\n" pp x pp (abs x);
  printf "abs %a -> %a\n" pp z pp (abs z);

  [%expect{|
    4i + 3i -> 7i
    4i - 3i -> 1i
    4i * 3i -> 12i
    4i / 3i -> 1i
    4i % 3i -> 1i
    4i ** 3i -> 64i
    4i // 3i -> 1.33
    -(4i) -> -4i
    ~-(4i) -> -4i
    +(-2i) -> -2i
    ~+(-2i) -> -2i
    neg 4i -> -4i
    neg -2i -> 2i
    abs 4i -> 4i
    abs -2i -> 2i
    |}]

let%expect_test "rel" =
  let open Format in
  let fn x y = begin
    printf "cmp %a %a -> %a\n" pp x pp y Cmp.pp (cmp x y);
    printf "%a >= %a -> %b\n" pp x pp y (x >= y);
    printf "%a <= %a -> %b\n" pp x pp y (x <= y);
    printf "%a = %a -> %b\n" pp x pp y (x = y);
    printf "%a > %a -> %b\n" pp x pp y (x > y);
    printf "%a < %a -> %b\n" pp x pp y (x < y);
    printf "%a <> %a -> %b\n" pp x pp y (x <> y);
    printf "ascending %a %a -> %a\n" pp x pp y Cmp.pp (ascending x y);
    printf "descending %a %a -> %a\n" pp x pp y Cmp.pp (descending x y);
  end in
  fn ~-(kv 1) (kv 0);
  printf "\n";
  fn (kv 0) (kv 0);
  printf "\n";
  fn (kv 1) (kv 0);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp %a ~min:%a ~max:%a -> %a\n"
        pp t pp min pp max pp (clamp t ~min ~max);
    printf "between %a ~low:%a ~high:%a -> %b\n"
        pp t pp min pp max (between t ~low:min ~high:max);
  end in
  fn2 ~-(kv 2) ~-(kv 1) (kv 1);
  fn2 ~-(kv 1) ~-(kv 1) (kv 1);
  fn2 (kv 0) ~-(kv 1) (kv 1);
  fn2 (kv 1) ~-(kv 1) (kv 1);
  fn2 (kv 2) ~-(kv 1) (kv 1);

  [%expect{|
    cmp -1i 0i -> Lt
    -1i >= 0i -> false
    -1i <= 0i -> true
    -1i = 0i -> false
    -1i > 0i -> false
    -1i < 0i -> true
    -1i <> 0i -> true
    ascending -1i 0i -> Lt
    descending -1i 0i -> Gt

    cmp 0i 0i -> Eq
    0i >= 0i -> true
    0i <= 0i -> true
    0i = 0i -> true
    0i > 0i -> false
    0i < 0i -> false
    0i <> 0i -> false
    ascending 0i 0i -> Eq
    descending 0i 0i -> Eq

    cmp 1i 0i -> Gt
    1i >= 0i -> true
    1i <= 0i -> false
    1i = 0i -> false
    1i > 0i -> true
    1i < 0i -> false
    1i <> 0i -> true
    ascending 1i 0i -> Gt
    descending 1i 0i -> Lt

    clamp -2i ~min:-1i ~max:1i -> -1i
    between -2i ~low:-1i ~high:1i -> false

    clamp -1i ~min:-1i ~max:1i -> -1i
    between -1i ~low:-1i ~high:1i -> true

    clamp 0i ~min:-1i ~max:1i -> 0i
    between 0i ~low:-1i ~high:1i -> true

    clamp 1i ~min:-1i ~max:1i -> 1i
    between 1i ~low:-1i ~high:1i -> true

    clamp 2i ~min:-1i ~max:1i -> 1i
    between 2i ~low:-1i ~high:1i -> false
    |}]

let%expect_test "min_max" =
  let open Format in
  let fn x y = begin
    printf "min %a %a -> %a\n" pp x pp y pp (min x y);
    printf "max %a %a -> %a\n" pp x pp y pp (max x y);
  end in
  fn ~-(kv 1) (kv 0);
  printf "\n";
  fn (kv 0) (kv 0);
  printf "\n";
  fn (kv 1) (kv 0);

  [%expect{|
    min -1i 0i -> -1i
    max -1i 0i -> 0i

    min 0i 0i -> 0i
    max 0i 0i -> 0i

    min 1i 0i -> 0i
    max 1i 0i -> 1i
    |}]
