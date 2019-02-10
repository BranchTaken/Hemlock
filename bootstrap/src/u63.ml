(* Partial Rudiments. *)
module Int = I63
type int = I63.t

module T = struct
  type t = int
  let num_bits = Sys.int_size
end
include T
include Intnb.Make_u(T)

let to_int t =
  t

let of_int x =
  x

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "limits" =
  let open Printf in

  printf "num_bits=%d\n" num_bits;
  printf "min_value=0x%x\n" min_value;
  printf "max_value=0x%x\n" max_value;

  [%expect{|
    num_bits=63
    min_value=0x0
    max_value=0x7fffffffffffffff
    |}]

let%expect_test "rel" =
  let open Printf in
  let fn x y = begin
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
  fn 0 0x4000_0000_0000_0000;
  printf "\n";
  fn 0 0x7fff_ffff_ffff_ffff;
  printf "\n";
  fn 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff;
  let fn2 t min max = begin
    printf "\n";
    printf "clamp 0x%x ~min:0x%x ~max:0x%x -> 0x%x\n" t min max (clamp t ~min
        ~max);
    printf "between 0x%x ~low:0x%x ~high:0x%x -> %b\n" t min max (between t
        ~low:min ~high:max);
  end in
  fn2 0x3fff_ffff_ffff_fffe 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x3fff_ffff_ffff_ffff 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0001 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0002 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;

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
  printf "max_value + 1 -> 0x%x\n" (max_value + 1);
  printf "min_value - 1 -> 0x%x\n" (min_value - 1);
  printf "max_value * 15 -> 0x%x\n" (max_value * 15);

  [%expect{|
    max_value + 1 -> 0x0
    min_value - 1 -> 0x7fffffffffffffff
    max_value * 15 -> 0x7ffffffffffffff1
    |}]
