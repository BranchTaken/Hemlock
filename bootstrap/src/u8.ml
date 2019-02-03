module T = struct
  type t = I63.t
  let num_bits = 8
end
include T
include Intnb.Make_u(T)

let of_int x =
  narrow_of_signed x

let to_int t =
  t

let of_uint x =
  narrow_of_unsigned x

let to_uint t =
  t

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "limits" =
  let open Printf in

  printf "num_bits=%d\n" num_bits;
  printf "min_value=0x%x\n" min_value;
  printf "max_value=0x%x\n" max_value;

  [%expect{|
    num_bits=8
    min_value=0x0
    max_value=0xff
    |}]

let%expect_test "rel" =
  let open Printf in
  let lambda x y = begin
    printf "cmp 0x%x 0x%x -> %s\n"
      x y (Sexplib.Sexp.to_string (Cmp.sexp_of_t (cmp x y)));
    printf "0x%x >= 0x%x -> %b\n" x y (x >= y);
    printf "0x%x <= 0x%x -> %b\n" x y (x <= y);
    printf "0x%x = 0x%x -> %b\n" x y (x = y);
    printf "0x%x > 0x%x -> %b\n" x y (x > y);
    printf "0x%x < 0x%x -> %b\n" x y (x < y);
    printf "0x%x <> 0x%x -> %b\n" x y (x <> y);
    printf "ascending 0x%x 0x%x -> %s\n"
      x y (Sexplib.Sexp.to_string (Cmp.sexp_of_t (ascending x y)));
    printf "descending 0x%x 0x%x -> %s\n"
      x y (Sexplib.Sexp.to_string (Cmp.sexp_of_t (descending x y)));
  end in
  lambda 0 0x80;
  printf "\n";
  lambda 0 0xff;
  printf "\n";
  lambda 0x80 0xff;
  let lambda2 t min max = begin
    printf "\n";
    printf "clamp 0x%x ~min:0x%x ~max:0x%x -> 0x%x\n" t min max (clamp t ~min
        ~max);
    printf "between 0x%x ~low:0x%x ~high:0x%x -> %b\n" t min max (between t
        ~low:min ~high:max);
  end in
  lambda2 0x7e 0x7f 0x81;
  lambda2 0x7f 0x7f 0x81;
  lambda2 0x80 0x7f 0x81;
  lambda2 0x81 0x7f 0x81;
  lambda2 0x82 0x7f 0x81;

  [%expect{|
    cmp 0x0 0x80 -> Lt
    0x0 >= 0x80 -> false
    0x0 <= 0x80 -> true
    0x0 = 0x80 -> false
    0x0 > 0x80 -> false
    0x0 < 0x80 -> true
    0x0 <> 0x80 -> true
    ascending 0x0 0x80 -> Lt
    descending 0x0 0x80 -> Gt

    cmp 0x0 0xff -> Lt
    0x0 >= 0xff -> false
    0x0 <= 0xff -> true
    0x0 = 0xff -> false
    0x0 > 0xff -> false
    0x0 < 0xff -> true
    0x0 <> 0xff -> true
    ascending 0x0 0xff -> Lt
    descending 0x0 0xff -> Gt

    cmp 0x80 0xff -> Lt
    0x80 >= 0xff -> false
    0x80 <= 0xff -> true
    0x80 = 0xff -> false
    0x80 > 0xff -> false
    0x80 < 0xff -> true
    0x80 <> 0xff -> true
    ascending 0x80 0xff -> Lt
    descending 0x80 0xff -> Gt

    clamp 0x7e ~min:0x7f ~max:0x81 -> 0x7f
    between 0x7e ~low:0x7f ~high:0x81 -> false

    clamp 0x7f ~min:0x7f ~max:0x81 -> 0x7f
    between 0x7f ~low:0x7f ~high:0x81 -> true

    clamp 0x80 ~min:0x7f ~max:0x81 -> 0x80
    between 0x80 ~low:0x7f ~high:0x81 -> true

    clamp 0x81 ~min:0x7f ~max:0x81 -> 0x81
    between 0x81 ~low:0x7f ~high:0x81 -> true

    clamp 0x82 ~min:0x7f ~max:0x81 -> 0x81
    between 0x82 ~low:0x7f ~high:0x81 -> false
    |}]

let%expect_test "wraparound" =
  let open Printf in
  printf "max_value + 1 -> 0x%x\n" (max_value + 1);
  printf "min_value - 1 -> 0x%x\n" (min_value - 1);
  printf "max_value * 15 -> 0x%x\n" (max_value * 15);

  [%expect{|
    max_value + 1 -> 0x0
    min_value - 1 -> 0xff
    max_value * 15 -> 0xf1
    |}]

let%expect_test "conversion" =
  let open Printf in
  let rec lambda = function
    | [] -> ()
    | x :: xs' -> begin
        let t = of_int x in
        let i = to_int t in
        let t' = of_int i in
        printf "of_int 0x%x -> to_int 0x%x -> of_int 0x%x -> 0x%x\n" x t i t';
        let t = of_uint x in
        let u = to_uint t in
        let t' = of_uint u in
        printf "of_uint 0x%x -> to_uint 0x%x -> of_uint 0x%x -> 0x%x\n"
          x t u t';
        lambda xs'
      end
  in
  lambda [-1; 0; 42; 127; 128; 255; 256; 257; max_int];

  [%expect{|
    of_int 0x7fffffffffffffff -> to_int 0xff -> of_int 0xff -> 0xff
    of_uint 0x7fffffffffffffff -> to_uint 0xff -> of_uint 0xff -> 0xff
    of_int 0x0 -> to_int 0x0 -> of_int 0x0 -> 0x0
    of_uint 0x0 -> to_uint 0x0 -> of_uint 0x0 -> 0x0
    of_int 0x2a -> to_int 0x2a -> of_int 0x2a -> 0x2a
    of_uint 0x2a -> to_uint 0x2a -> of_uint 0x2a -> 0x2a
    of_int 0x7f -> to_int 0x7f -> of_int 0x7f -> 0x7f
    of_uint 0x7f -> to_uint 0x7f -> of_uint 0x7f -> 0x7f
    of_int 0x80 -> to_int 0x80 -> of_int 0x80 -> 0x80
    of_uint 0x80 -> to_uint 0x80 -> of_uint 0x80 -> 0x80
    of_int 0xff -> to_int 0xff -> of_int 0xff -> 0xff
    of_uint 0xff -> to_uint 0xff -> of_uint 0xff -> 0xff
    of_int 0x100 -> to_int 0x0 -> of_int 0x0 -> 0x0
    of_uint 0x100 -> to_uint 0x0 -> of_uint 0x0 -> 0x0
    of_int 0x101 -> to_int 0x1 -> of_int 0x1 -> 0x1
    of_uint 0x101 -> to_uint 0x1 -> of_uint 0x1 -> 0x1
    of_int 0x3fffffffffffffff -> to_int 0xff -> of_int 0xff -> 0xff
    of_uint 0x3fffffffffffffff -> to_uint 0xff -> of_uint 0xff -> 0xff
    |}]
