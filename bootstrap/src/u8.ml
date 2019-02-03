module T = struct
  type t = int
  let num_bits = 8
end
include T
include Intnb.Make_u(T)

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
