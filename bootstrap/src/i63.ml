include Rudiments_int

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "float" =
  let open Printf in
  let x = of_float 0. in
  printf "%d\n" x;
  let f = to_float 1 in
  printf "%.1f\n" f;

  [%expect{|
    0
    1.0
    |}]

let%expect_test "string" =
  let open Printf in
  let x = of_string "0" in
  printf "%d\n" x;
  let s = to_string 1 in
  printf "%s\n" s;

  [%expect{|
    0
    1
    |}]

let%expect_test "sexp" =
  let open Printf in
  let t = 42 in
  let sexp = sexp_of_t 42 in
  printf "sexp_of_t %d -> %s\n" t (Sexplib.Sexp.to_string sexp);
  printf "t_of_sexp -> %d\n" (t_of_sexp sexp);

  [%expect{|
    sexp_of_t 42 -> 42
    t_of_sexp -> 42
    |}]

let%expect_test "limits" =
  let open Printf in

  printf "num_bits=%d\n" num_bits;
  printf "min_value=0x%x\n" min_value;
  printf "max_value=0x%x\n" max_value;

  [%expect{|
    num_bits=63
    min_value=0x4000000000000000
    max_value=0x3fffffffffffffff
    |}]

let%expect_test "constants" =
  let open Printf in
  printf "zero=%d\n" zero;
  printf "one=%d\n" one;
  printf "neg_one=%d\n" neg_one;

  [%expect{|
    zero=0
    one=1
    neg_one=-1
    |}]

let%expect_test "is_" =
  let open Printf in
  let ns = [-1; 0; 1] in
  let rec fn ns = begin
    match ns with
    | [] -> ()
    | n :: ns' -> begin
        printf "%d\n" n;
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
    -1
      is_positive    =false
      is_non_negative=false
      is_negative    =true
      is_non_positive=true

    0
      is_positive    =false
      is_non_negative=true
      is_negative    =false
      is_non_positive=true

    1
      is_positive    =true
      is_non_negative=true
      is_negative    =false
      is_non_positive=false
    |}]

let%expect_test "seq" =
  let open Printf in
  let x = 1 in
  printf "succ: %d -> %d\n" x (succ x);
  printf "pred: %d -> %d\n" x (pred x);

  [%expect{|
    succ: 1 -> 2
    pred: 1 -> 0
    |}]

let%expect_test "bit_" =
  let open Printf in

  let x = 0b0011 in
  let y = 0b0101 in
  printf "bit_and 0x%x 0x%x -> 0x%x\n" x y (bit_and x y);
  printf "bit_or 0x%x 0x%x -> 0x%x\n" x y (bit_or x y);
  printf "bit_xor 0x%x 0x%x -> 0x%x\n" x y (bit_xor x y);

  let x = 0b10 in
  printf "bit_not 0x%x -> 0x%x\n" x (bit_not x);

  let x = 0xff in
  let s = 4 in
  printf "bit_sl 0x%x %d -> 0x%x\n" x s (bit_sl x s);

  let x = -1 in
  let s = 4 in
  printf "bit_usr 0x%x %d -> 0x%x\n" x s (bit_usr x s);
  printf "bit_ssr 0x%x %d -> 0x%x\n" x s (bit_ssr x s);

  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_pop 0x%x -> %d\n" x (bit_pop x);
        printf "bit_clz 0x%x -> %d\n" x (bit_clz x);
        printf "bit_ctz 0x%x -> %d\n" x (bit_ctz x);
        fn xs'
      end
  end in
  fn [-1; 0; 1; min_value; max_value; 0xf73100];

  [%expect{|
    bit_and 0x3 0x5 -> 0x1
    bit_or 0x3 0x5 -> 0x7
    bit_xor 0x3 0x5 -> 0x6
    bit_not 0x2 -> 0x7ffffffffffffffd
    bit_sl 0xff 4 -> 0xff0
    bit_usr 0x7fffffffffffffff 4 -> 0x7ffffffffffffff
    bit_ssr 0x7fffffffffffffff 4 -> 0x7fffffffffffffff
    bit_pop 0x7fffffffffffffff -> 63
    bit_clz 0x7fffffffffffffff -> 0
    bit_ctz 0x7fffffffffffffff -> 0
    bit_pop 0x0 -> 0
    bit_clz 0x0 -> 63
    bit_ctz 0x0 -> 63
    bit_pop 0x1 -> 1
    bit_clz 0x1 -> 62
    bit_ctz 0x1 -> 0
    bit_pop 0x4000000000000000 -> 1
    bit_clz 0x4000000000000000 -> 0
    bit_ctz 0x4000000000000000 -> 62
    bit_pop 0x3fffffffffffffff -> 62
    bit_clz 0x3fffffffffffffff -> 1
    bit_ctz 0x3fffffffffffffff -> 0
    bit_pop 0xf73100 -> 10
    bit_clz 0xf73100 -> 39
    bit_ctz 0xf73100 -> 8
    |}]

let%expect_test "pow2_lg" =
  let open Printf in
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "is_pow2 0x%x -> %b\n" x (is_pow2 x);
        printf "floor_pow2 0x%x -> 0x%x\n" x (floor_pow2 x);
        printf "ceil_pow2 0x%x -> 0x%x\n" x (ceil_pow2 x);
        if x > 0 then begin
          printf "floor_lg 0x%x -> %d\n" x (floor_lg x);
          printf "ceil_lg 0x%x -> %d\n" x (ceil_lg x)
        end;
        fn xs'
      end
  end in
  fn [0; 1; 2; 3; 4; 0xf0; max_value];

  [%expect{|
    is_pow2 0x0 -> true
    floor_pow2 0x0 -> 0x0
    ceil_pow2 0x0 -> 0x0
    is_pow2 0x1 -> true
    floor_pow2 0x1 -> 0x1
    ceil_pow2 0x1 -> 0x1
    floor_lg 0x1 -> 0
    ceil_lg 0x1 -> 0
    is_pow2 0x2 -> true
    floor_pow2 0x2 -> 0x2
    ceil_pow2 0x2 -> 0x2
    floor_lg 0x2 -> 1
    ceil_lg 0x2 -> 1
    is_pow2 0x3 -> false
    floor_pow2 0x3 -> 0x2
    ceil_pow2 0x3 -> 0x4
    floor_lg 0x3 -> 1
    ceil_lg 0x3 -> 2
    is_pow2 0x4 -> true
    floor_pow2 0x4 -> 0x4
    ceil_pow2 0x4 -> 0x4
    floor_lg 0x4 -> 2
    ceil_lg 0x4 -> 2
    is_pow2 0xf0 -> false
    floor_pow2 0xf0 -> 0x80
    ceil_pow2 0xf0 -> 0x100
    floor_lg 0xf0 -> 7
    ceil_lg 0xf0 -> 8
    is_pow2 0x3fffffffffffffff -> false
    floor_pow2 0x3fffffffffffffff -> 0x2000000000000000
    ceil_pow2 0x3fffffffffffffff -> 0x4000000000000000
    floor_lg 0x3fffffffffffffff -> 61
    ceil_lg 0x3fffffffffffffff -> 62
    |}]

let%expect_test "ops" =
  let open Printf in
  let x = 4 in
  let y = 3 in
  printf "%d + %d -> %d\n" x y (x + y);
  printf "%d - %d -> %d\n" x y (x - y);
  printf "%d * %d -> %d\n" x y (x * y);
  printf "%d / %d -> %d\n" x y (x / y);
  printf "%d %% %d -> %d\n" x y (x % y);
  printf "%d ** %d -> %d\n" x y (x ** y);
  printf "%d // %d -> %.2f\n" x y (x // y);
  let z = -2 in
  printf "-(%d) -> %d\n" x (-x);
  printf "~-(%d) -> %d\n" x ~-x;
  printf "+(%d) -> %d\n" z (+z);
  printf "~+(%d) -> %d\n" z ~+z;
  printf "neg %d -> %d\n" x (neg x);
  printf "neg %d -> %d\n" z (neg z);
  printf "abs %d -> %d\n" x (abs x);
  printf "abs %d -> %d\n" z (abs z);

  [%expect{|
    4 + 3 -> 7
    4 - 3 -> 1
    4 * 3 -> 12
    4 / 3 -> 1
    4 % 3 -> 1
    4 ** 3 -> 64
    4 // 3 -> 1.33
    -(4) -> -4
    ~-(4) -> -4
    +(-2) -> -2
    ~+(-2) -> -2
    neg 4 -> -4
    neg -2 -> 2
    abs 4 -> 4
    abs -2 -> 2
    |}]

let%expect_test "rel" =
  let open Printf in
  let fn x y = begin
    printf "cmp %d %d -> %s\n"
      x y (Sexplib.Sexp.to_string (Cmp.sexp_of_t (cmp x y)));
    printf "%d >= %d -> %b\n" x y (x >= y);
    printf "%d <= %d -> %b\n" x y (x <= y);
    printf "%d = %d -> %b\n" x y (x = y);
    printf "%d > %d -> %b\n" x y (x > y);
    printf "%d < %d -> %b\n" x y (x < y);
    printf "%d <> %d -> %b\n" x y (x <> y);
    printf "ascending %d %d -> %s\n"
      x y (Sexplib.Sexp.to_string (Cmp.sexp_of_t (ascending x y)));
    printf "descending %d %d -> %s\n"
      x y (Sexplib.Sexp.to_string (Cmp.sexp_of_t (descending x y)));
  end in
  fn ~-1 0;
  printf "\n";
  fn 0 0;
  printf "\n";
  fn 1 0;
  let fn2 t min max = begin
    printf "\n";
    printf "clamp %d ~min:%d ~max:%d -> %d\n" t min max (clamp t ~min ~max);
    printf "between %d ~low:%d ~high:%d -> %b\n" t min max (between t ~low:min
        ~high:max);
  end in
  fn2 ~-2 ~-1 1;
  fn2 ~-1 ~-1 1;
  fn2 0 ~-1 1;
  fn2 1 ~-1 1;
  fn2 2 ~-1 1;

  [%expect{|
    cmp -1 0 -> Lt
    -1 >= 0 -> false
    -1 <= 0 -> true
    -1 = 0 -> false
    -1 > 0 -> false
    -1 < 0 -> true
    -1 <> 0 -> true
    ascending -1 0 -> Lt
    descending -1 0 -> Gt

    cmp 0 0 -> Eq
    0 >= 0 -> true
    0 <= 0 -> true
    0 = 0 -> true
    0 > 0 -> false
    0 < 0 -> false
    0 <> 0 -> false
    ascending 0 0 -> Eq
    descending 0 0 -> Eq

    cmp 1 0 -> Gt
    1 >= 0 -> true
    1 <= 0 -> false
    1 = 0 -> false
    1 > 0 -> true
    1 < 0 -> false
    1 <> 0 -> true
    ascending 1 0 -> Gt
    descending 1 0 -> Lt

    clamp -2 ~min:-1 ~max:1 -> -1
    between -2 ~low:-1 ~high:1 -> false

    clamp -1 ~min:-1 ~max:1 -> -1
    between -1 ~low:-1 ~high:1 -> true

    clamp 0 ~min:-1 ~max:1 -> 0
    between 0 ~low:-1 ~high:1 -> true

    clamp 1 ~min:-1 ~max:1 -> 1
    between 1 ~low:-1 ~high:1 -> true

    clamp 2 ~min:-1 ~max:1 -> 1
    between 2 ~low:-1 ~high:1 -> false
    |}]

let%expect_test "min_max" =
  let open Printf in
  let fn x y = begin
    printf "min %d %d -> %d\n" x y (min x y);
    printf "max %d %d -> %d\n" x y (max x y);
  end in
  fn ~-1 0;
  printf "\n";
  fn 0 0;
  printf "\n";
  fn 1 0;

  [%expect{|
    min -1 0 -> -1
    max -1 0 -> 0

    min 0 0 -> 0
    max 0 0 -> 0

    min 1 0 -> 0
    max 1 0 -> 1
    |}]
