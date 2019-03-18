open Rudiments_uint

module T = struct
  type t = uint
  let num_bits = uint_of_int Sys.int_size
end
include T
include Intnb.Make_u(T)

let to_int t =
  int_of_uint t

let of_int x =
  uint_of_int x

let kv x =
  of_int x

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "limits" =
  let open Printf in

  printf "num_bits=%u\n" (to_int num_bits);
  printf "min_value=0x%x\n" (to_int min_value);
  printf "max_value=0x%x\n" (to_int max_value);

  [%expect{|
    num_bits=63
    min_value=0x0
    max_value=0x7fffffffffffffff
    |}]

let%expect_test "rel" =
  let open Printf in
  let fn x y = begin
    printf "cmp 0x%x 0x%x -> %s\n"
      (to_int x) (to_int y) (Sexplib.Sexp.to_string (Cmp.sexp_of_t (cmp x y)));
    printf "0x%x >= 0x%x -> %b\n" (to_int x) (to_int y) (x >= y);
    printf "0x%x <= 0x%x -> %b\n" (to_int x) (to_int y) (x <= y);
    printf "0x%x = 0x%x -> %b\n" (to_int x) (to_int y) (x = y);
    printf "0x%x > 0x%x -> %b\n" (to_int x) (to_int y) (x > y);
    printf "0x%x < 0x%x -> %b\n" (to_int x) (to_int y) (x < y);
    printf "0x%x <> 0x%x -> %b\n" (to_int x) (to_int y) (x <> y);
    printf "ascending 0x%x 0x%x -> %s\n" (to_int x) (to_int y)
      (Sexplib.Sexp.to_string (Cmp.sexp_of_t (ascending x y)));
    printf "descending 0x%x 0x%x -> %s\n" (to_int x) (to_int y)
      (Sexplib.Sexp.to_string (Cmp.sexp_of_t (descending x y)));
  end in
  fn (kv 0) (kv 0x4000_0000_0000_0000);
  printf "\n";
  fn (kv 0) (kv 0x7fff_ffff_ffff_ffff);
  printf "\n";
  fn (kv 0x4000_0000_0000_0000) (kv 0x3fff_ffff_ffff_ffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp 0x%x ~min:0x%x ~max:0x%x -> 0x%x\n"
      (to_int t) (to_int min) (to_int max) (to_int (clamp t ~min ~max));
    printf "between 0x%x ~low:0x%x ~high:0x%x -> %b\n"
      (to_int t) (to_int min) (to_int max) (between t ~low:min ~high:max);
  end in
  fn2 (kv 0x3fff_ffff_ffff_fffe) (kv 0x3fff_ffff_ffff_ffff) (kv
      0x4000_0000_0000_0001);
  fn2 (kv 0x3fff_ffff_ffff_ffff) (kv 0x3fff_ffff_ffff_ffff) (kv
      0x4000_0000_0000_0001);
  fn2 (kv 0x4000_0000_0000_0000) (kv 0x3fff_ffff_ffff_ffff) (kv
      0x4000_0000_0000_0001);
  fn2 (kv 0x4000_0000_0000_0001) (kv 0x3fff_ffff_ffff_ffff) (kv
      0x4000_0000_0000_0001);
  fn2 (kv 0x4000_0000_0000_0002) (kv 0x3fff_ffff_ffff_ffff) (kv
      0x4000_0000_0000_0001);

  [%expect{|
    cmp 0x0 0x4000000000000000 -> Lt
    0x0 >= 0x4000000000000000 -> false
    0x0 <= 0x4000000000000000 -> true
    0x0 = 0x4000000000000000 -> false
    0x0 > 0x4000000000000000 -> false
    0x0 < 0x4000000000000000 -> true
    0x0 <> 0x4000000000000000 -> true
    ascending 0x0 0x4000000000000000 -> Lt
    descending 0x0 0x4000000000000000 -> Gt

    cmp 0x0 0x7fffffffffffffff -> Lt
    0x0 >= 0x7fffffffffffffff -> false
    0x0 <= 0x7fffffffffffffff -> true
    0x0 = 0x7fffffffffffffff -> false
    0x0 > 0x7fffffffffffffff -> false
    0x0 < 0x7fffffffffffffff -> true
    0x0 <> 0x7fffffffffffffff -> true
    ascending 0x0 0x7fffffffffffffff -> Lt
    descending 0x0 0x7fffffffffffffff -> Gt

    cmp 0x4000000000000000 0x3fffffffffffffff -> Gt
    0x4000000000000000 >= 0x3fffffffffffffff -> true
    0x4000000000000000 <= 0x3fffffffffffffff -> false
    0x4000000000000000 = 0x3fffffffffffffff -> false
    0x4000000000000000 > 0x3fffffffffffffff -> true
    0x4000000000000000 < 0x3fffffffffffffff -> false
    0x4000000000000000 <> 0x3fffffffffffffff -> true
    ascending 0x4000000000000000 0x3fffffffffffffff -> Gt
    descending 0x4000000000000000 0x3fffffffffffffff -> Lt

    clamp 0x3ffffffffffffffe ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x3fffffffffffffff
    between 0x3ffffffffffffffe ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> false

    clamp 0x3fffffffffffffff ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x3fffffffffffffff
    between 0x3fffffffffffffff ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> true

    clamp 0x4000000000000000 ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x4000000000000000
    between 0x4000000000000000 ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> true

    clamp 0x4000000000000001 ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x4000000000000001
    between 0x4000000000000001 ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> true

    clamp 0x4000000000000002 ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x4000000000000001
    between 0x4000000000000002 ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> false
    |}]

let%expect_test "narrowing" =
  let open Printf in
  printf "max_value + 1 -> 0x%x\n" (to_int (max_value + (kv 1)));
  printf "min_value - 1 -> 0x%x\n" (to_int (min_value - (kv 1)));
  printf "max_value * 15 -> 0x%x\n" (to_int (max_value * (kv 15)));

  [%expect{|
    max_value + 1 -> 0x0
    min_value - 1 -> 0x7fffffffffffffff
    max_value * 15 -> 0x7ffffffffffffff1
    |}]
