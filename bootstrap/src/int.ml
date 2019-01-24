module T = struct
  type t = int

  let cmp t0 t1 =
    let rel = compare t0 t1 in
    if rel < 0 then
      Cmp.Lt
    else if rel = 0 then
      Cmp.Eq
    else
      Cmp.Gt
end
include T
include Cmpable.Make_rel(T)

let of_float f =
  int_of_float f

let to_float t =
  float_of_int t

let of_string s =
  int_of_string s

let to_string t =
  string_of_int t

let num_bits = Sys.int_size

let min_value = min_int

let max_value = max_int

let zero = 0

let one = 1

let minus_one = (-1)

let is_positive t =
  t > 0

let is_non_negative t =
  t >= 0

let is_negative t =
  t < 0

let is_non_positive t =
  t <= 0

let succ t =
  t + 1

let pred t =
  t - 1

let bit_and t0 t1 =
  t0 land t1

let bit_or t0 t1 =
  t0 lor t1

let bit_xor t0 t1 =
  t0 lxor t1

let bit_not t =
  lnot t

let bit_sl t i =
  t lsl i

let bit_usr t i =
  t lsr i

let bit_ssr t i =
  t asr i

let bit_pop t =
  let x = t in
  let x = x - (bit_and (bit_usr x 1) 0x5555_5555_5555_5555) in
  let c3s = 0x3333_3333_3333_3333 in
  let x = (bit_and x c3s) + (bit_and (bit_usr x 2) c3s) in
  let x = bit_and (x + (bit_usr x 4)) 0x0f0f_0f0f_0f0f_0f0f in
  let x = x + (bit_usr x 8) in
  let x = x + (bit_usr x 16) in
  let x = x + (bit_usr x 32) in
  bit_and x 0x3f

let bit_clz t =
  let x = t in
  let x = bit_or x (bit_usr x 1) in
  let x = bit_or x (bit_usr x 2) in
  let x = bit_or x (bit_usr x 4) in
  let x = bit_or x (bit_usr x 8) in
  let x = bit_or x (bit_usr x 16) in
  let x = bit_or x (bit_usr x 32) in
  bit_pop (bit_not x)

let bit_ctz t =
  bit_pop (bit_and (bit_not t) (t - 1))

let is_pow2 t =
  assert (t >= 0);
  (bit_and t (t - 1)) = 0

let floor_pow2 t =
  assert (t >= 0);
  if t <= 1 then t
  else bit_sl 1 (num_bits - 1 - (bit_clz t))

let ceil_pow2 t =
  assert (t >= 0);
  if t <= 1 then t
  else bit_sl 1 (num_bits - (bit_clz (t - 1)))

let floor_lg t =
  assert (t > 0);
  num_bits - 1 - (bit_clz t)

let ceil_lg t =
  assert (t > 0);
  floor_lg t + (if is_pow2 t then 0 else 1)

let ( + ) t0 t1 =
  t0 + t1

let ( - ) t0 t1 =
  t0 - t1

let ( * ) t0 t1 =
  t0 * t1

let ( / ) t0 t1 =
  t0 / t1

let ( % ) t0 t1 =
  t0 mod t1

let ( ** ) t0 t1 =
  (* Decompose the exponent to limit algorithmic complexity. *)
  let neg, n = if t1 < 0 then
      true, -t1
    else
      false, t1
  in
  let rec lambda r p n = begin
    match n with
    | 0 -> r
    | _ -> begin
        let r' = match bit_and n 1 with
          | 0 -> r
          | 1 -> r * p
          | _ -> assert false
        in
        let p' = p * p in
        let n' = bit_usr n 1 in
        lambda r' p' n'
      end
  end in
  let r = lambda 1 t0 n in
  match neg with
    | false -> r
    | true -> 1 / r

let ( // ) t0 t1 =
  (to_float t0) /. (to_float t1)

let abs t =
  abs t

let neg t =
  -t

let min t0 t1 =
  match cmp t0 t1 with
  | Lt | Eq -> t0
  | Gt -> t1

let max t0 t1 =
  match cmp t0 t1 with
  | Lt | Eq -> t1
  | Gt -> t0

let compare t0 t1 =
  match cmp t0 t1 with
  | Cmp.Lt -> -1
  | Cmp.Eq -> 0
  | Cmp.Gt -> 1

let sexp_of_t t =
  Sexplib.Std.sexp_of_int t

let t_of_sexp sexp =
  Sexplib.Std.int_of_sexp sexp

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
  printf "minus_one=%d\n" minus_one;

  [%expect{|
    zero=0
    one=1
    minus_one=-1
    |}]

let%expect_test "is_" =
  let open Printf in
  let ns = [-1; 0; 1] in
  let rec lambda ns = begin
    match ns with
    | [] -> ()
    | n :: ns' -> begin
        printf "%d\n" n;
        printf "  is_positive    =%b\n" (is_positive n);
        printf "  is_non_negative=%b\n" (is_non_negative n);
        printf "  is_negative    =%b\n" (is_negative n);
        printf "  is_non_positive=%b\n" (is_non_positive n);
        printf "\n";
        lambda ns'
      end
  end in
  lambda ns;

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

  let rec lambda xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_pop 0x%x -> %d\n" x (bit_pop x);
        printf "bit_clz 0x%x -> %d\n" x (bit_clz x);
        printf "bit_ctz 0x%x -> %d\n" x (bit_ctz x);
        lambda xs'
      end
  end in
  lambda [-1; 0; 1; min_value; max_value; 0xf73100];

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
  let rec lambda xs = begin
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
        lambda xs'
      end
  end in
  lambda [0; 1; 2; 3; 4; 0xf0; max_value];

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

  [%expect{|
    4 + 3 -> 7
    4 - 3 -> 1
    4 * 3 -> 12
    4 / 3 -> 1
    4 % 3 -> 1
    4 ** 3 -> 64
    4 // 3 -> 1.33
    |}]

let%expect_test "cmp" =
  let open Printf in
  let lambda x y = begin
    printf "cmp %d %d -> %s\n" x y (match cmp x y with
      | Lt -> "Lt"
      | Eq -> "Eq"
      | Gt -> "Gt"
    );
    printf "%d >= %d -> %b\n" x y (x >= y);
    printf "%d <= %d -> %b\n" x y (x <= y);
    printf "%d = %d -> %b\n" x y (x = y);
    printf "%d > %d -> %b\n" x y (x > y);
    printf "%d < %d -> %b\n" x y (x < y);
    printf "%d <> %d -> %b\n" x y (x <> y)
  end in
  lambda ~-1 0;
  printf "\n";
  lambda 0 0;
  printf "\n";
  lambda 1 0;

  [%expect{|
    cmp -1 0 -> Lt
    -1 >= 0 -> false
    -1 <= 0 -> true
    -1 = 0 -> false
    -1 > 0 -> false
    -1 < 0 -> true
    -1 <> 0 -> true

    cmp 0 0 -> Eq
    0 >= 0 -> true
    0 <= 0 -> true
    0 = 0 -> true
    0 > 0 -> false
    0 < 0 -> false
    0 <> 0 -> false

    cmp 1 0 -> Gt
    1 >= 0 -> true
    1 <= 0 -> false
    1 = 0 -> false
    1 > 0 -> true
    1 < 0 -> false
    1 <> 0 -> true
    |}]

let%expect_test "min_max" =
  let open Printf in
  let lambda x y = begin
    printf "min %d %d -> %d\n" x y (min x y);
    printf "max %d %d -> %d\n" x y (max x y);
  end in
  lambda ~-1 0;
  printf "\n";
  lambda 0 0;
  printf "\n";
  lambda 1 0;

  [%expect{|
    min -1 0 -> -1
    max -1 0 -> 0

    min 0 0 -> 0
    max 0 0 -> 0

    min 1 0 -> 0
    max 1 0 -> 1
    |}]
