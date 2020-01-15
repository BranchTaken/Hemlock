(* Partial Rudiments. *)
module Int = I63
module Uint = U63
module Codepoint = U21
type codepoint = Codepoint.t
open Rudiments_uint
open Rudiments_functions

module T = struct
  type t = uint
  let num_bits = (Uint.kv 8)
end
include T
include Intnb.Make_u(T)

let to_int t =
  Uint.to_int t

let of_int x =
  narrow_of_signed x

let of_int_hlt x =
  let t = of_int x in
  let x' = to_int t in
  match Int.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let kv x =
  of_int x

let to_uint t =
  t

let of_uint x =
  narrow_of_unsigned x

let of_uint_hlt x =
  let t = of_uint x in
  let x' = to_uint t in
  match Uint.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let to_codepoint t =
  Codepoint.of_uint (to_uint t)

let of_codepoint x =
  narrow_of_unsigned (Codepoint.to_uint x)

let of_codepoint_hlt x =
  let t = of_codepoint x in
  let x' = to_codepoint (to_uint t) in
  match Codepoint.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "limits" =
  let open Printf in

  printf "num_bits=%u\n" (Uint.to_int num_bits);
  printf "min_value=0x%x\n" (to_int min_value);
  printf "max_value=0x%x\n" (to_int max_value);

  [%expect{|
    num_bits=8
    min_value=0x0
    max_value=0xff
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
  fn (kv 0) (kv 0x80);
  printf "\n";
  fn (kv 0) (kv 0xff);
  printf "\n";
  fn (kv 0x80) (kv 0xff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp 0x%x ~min:0x%x ~max:0x%x -> 0x%x\n"
      (to_int t) (to_int min) (to_int max) (to_int (clamp t ~min ~max));
    printf "between 0x%x ~low:0x%x ~high:0x%x -> %b\n"
      (to_int t) (to_int min) (to_int max) (between t
        ~low:min ~high:max);
  end in
  fn2 (kv 0x7e) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x7f) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x80) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x81) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x82) (kv 0x7f) (kv 0x81);

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
  printf "max_value + 1 -> 0x%x\n" (to_int (max_value + (kv 1)));
  printf "min_value - 1 -> 0x%x\n" (to_int (min_value - (kv 1)));
  printf "max_value * 15 -> 0x%x\n" (to_int (max_value * (kv 15)));

  [%expect{|
    max_value + 1 -> 0x0
    min_value - 1 -> 0xff
    max_value * 15 -> 0xf1
    |}]

let%expect_test "conversion" =
  let open Printf in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let t = of_int x in
        let i = to_int t in
        let t' = of_int i in
        printf "of_int 0x%x -> to_int 0x%x -> of_int 0x%x -> 0x%x\n"
          x (to_int t) i (to_int t');
        let t = of_uint (Uint.kv x) in
        let u = to_uint t in
        let t' = of_uint u in
        printf "of_uint 0x%x -> to_uint 0x%x -> of_uint 0x%x -> 0x%x\n"
          x (to_int t) (Uint.to_int u) (to_int t');

        let c = U21.of_uint (Uint.of_int x) in
        let t = of_codepoint c in
        let c' = to_codepoint t in
        let t' = of_codepoint c' in
        printf ("Codepoint.of_uint 0x%x -> of_codepoint 0x%x -> " ^^
          "to_codepoint 0x%x -> of_codepoint 0x%x -> 0x%x\n") x
          (Codepoint.to_int c) (to_int t) (Codepoint.to_int c') (to_int t');

        fn xs'
      end
  in
  fn [-1; 0; 42; 127; 128; 255; 256; 257; max_int];

  [%expect{|
    of_int 0x7fffffffffffffff -> to_int 0xff -> of_int 0xff -> 0xff
    of_uint 0x7fffffffffffffff -> to_uint 0xff -> of_uint 0xff -> 0xff
    Codepoint.of_uint 0x7fffffffffffffff -> of_codepoint 0x1fffff -> to_codepoint 0xff -> of_codepoint 0xff -> 0xff
    of_int 0x0 -> to_int 0x0 -> of_int 0x0 -> 0x0
    of_uint 0x0 -> to_uint 0x0 -> of_uint 0x0 -> 0x0
    Codepoint.of_uint 0x0 -> of_codepoint 0x0 -> to_codepoint 0x0 -> of_codepoint 0x0 -> 0x0
    of_int 0x2a -> to_int 0x2a -> of_int 0x2a -> 0x2a
    of_uint 0x2a -> to_uint 0x2a -> of_uint 0x2a -> 0x2a
    Codepoint.of_uint 0x2a -> of_codepoint 0x2a -> to_codepoint 0x2a -> of_codepoint 0x2a -> 0x2a
    of_int 0x7f -> to_int 0x7f -> of_int 0x7f -> 0x7f
    of_uint 0x7f -> to_uint 0x7f -> of_uint 0x7f -> 0x7f
    Codepoint.of_uint 0x7f -> of_codepoint 0x7f -> to_codepoint 0x7f -> of_codepoint 0x7f -> 0x7f
    of_int 0x80 -> to_int 0x80 -> of_int 0x80 -> 0x80
    of_uint 0x80 -> to_uint 0x80 -> of_uint 0x80 -> 0x80
    Codepoint.of_uint 0x80 -> of_codepoint 0x80 -> to_codepoint 0x80 -> of_codepoint 0x80 -> 0x80
    of_int 0xff -> to_int 0xff -> of_int 0xff -> 0xff
    of_uint 0xff -> to_uint 0xff -> of_uint 0xff -> 0xff
    Codepoint.of_uint 0xff -> of_codepoint 0xff -> to_codepoint 0xff -> of_codepoint 0xff -> 0xff
    of_int 0x100 -> to_int 0x0 -> of_int 0x0 -> 0x0
    of_uint 0x100 -> to_uint 0x0 -> of_uint 0x0 -> 0x0
    Codepoint.of_uint 0x100 -> of_codepoint 0x100 -> to_codepoint 0x0 -> of_codepoint 0x0 -> 0x0
    of_int 0x101 -> to_int 0x1 -> of_int 0x1 -> 0x1
    of_uint 0x101 -> to_uint 0x1 -> of_uint 0x1 -> 0x1
    Codepoint.of_uint 0x101 -> of_codepoint 0x101 -> to_codepoint 0x1 -> of_codepoint 0x1 -> 0x1
    of_int 0x3fffffffffffffff -> to_int 0xff -> of_int 0xff -> 0xff
    of_uint 0x3fffffffffffffff -> to_uint 0xff -> of_uint 0xff -> 0xff
    Codepoint.of_uint 0x3fffffffffffffff -> of_codepoint 0x1fffff -> to_codepoint 0xff -> of_codepoint 0xff -> 0xff
    |}]
