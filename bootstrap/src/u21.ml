(* Partial Rudiments. *)
module Uint = U63
module Int = I63
open Rudiments_uint
open Rudiments_functions

module T = struct
  type t = uint
  let num_bits = (Uint.kv 21)
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

let of_char c =
  of_int (Stdlib.Char.code c)

let nul = (kv 0x00)
let soh = (kv 0x01)
let stx = (kv 0x02)
let etx = (kv 0x03)
let eot = (kv 0x04)
let enq = (kv 0x05)
let ack = (kv 0x06)
let bel = (kv 0x07)
let bs = of_char '\b'
let ht = of_char '\t'
let lf = of_char '\n'
let nl = of_char '\n'
let vt = (kv 0x0b)
let ff = (kv 0x0c)
let cr = of_char '\r'
let so = (kv 0x0e)
let si = (kv 0x0f)
let dle = (kv 0x10)
let dc1 = (kv 0x11)
let dc2 = (kv 0x12)
let dc3 = (kv 0x13)
let dc4 = (kv 0x14)
let nak = (kv 0x15)
let syn = (kv 0x16)
let etb = (kv 0x17)
let can = (kv 0x18)
let em = (kv 0x19)
let sub = (kv 0x1a)
let esc = (kv 0x1b)
let fs = (kv 0x1c)
let gs = (kv 0x1d)
let rs = (kv 0x1e)
let us = (kv 0x1f)
let del = (kv 0x7f)

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "limits" =
  let open Printf in

  printf "num_bits=%u\n" (Uint.to_int num_bits);
  printf "min_value=0x%x\n" (to_int min_value);
  printf "max_value=0x%x\n" (to_int max_value);

  [%expect{|
    num_bits=21
    min_value=0x0
    max_value=0x1fffff
    |}]

let%expect_test "rel" =
  let open Printf in
  let fn x y = begin
    printf "cmp 0x%x 0x%x -> %s\n" (to_int x) (to_int y)
      (Sexplib.Sexp.to_string (Cmp.sexp_of_t (cmp x y)));
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
  fn (kv 0) (kv 0x10_0000);
  printf "\n";
  fn (kv 0) (kv 0x1f_ffff);
  printf "\n";
  fn (kv 0x10_0000) (kv 0x1f_ffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp 0x%x ~min:0x%x ~max:0x%x -> 0x%x\n"
      (to_int t) (to_int min) (to_int max) (to_int (clamp t ~min ~max));
    printf "between 0x%x ~low:0x%x ~high:0x%x -> %b\n"
      (to_int t) (to_int min) (to_int max) (between t ~low:min ~high:max);
  end in
  fn2 (kv 0x0f_fffe) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x0f_ffff) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0000) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0001) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0002) (kv 0x0f_ffff) (kv 0x10_0001);

  [%expect{|
    cmp 0x0 0x100000 -> Lt
    0x0 >= 0x100000 -> false
    0x0 <= 0x100000 -> true
    0x0 = 0x100000 -> false
    0x0 > 0x100000 -> false
    0x0 < 0x100000 -> true
    0x0 <> 0x100000 -> true
    ascending 0x0 0x100000 -> Lt
    descending 0x0 0x100000 -> Gt

    cmp 0x0 0x1fffff -> Lt
    0x0 >= 0x1fffff -> false
    0x0 <= 0x1fffff -> true
    0x0 = 0x1fffff -> false
    0x0 > 0x1fffff -> false
    0x0 < 0x1fffff -> true
    0x0 <> 0x1fffff -> true
    ascending 0x0 0x1fffff -> Lt
    descending 0x0 0x1fffff -> Gt

    cmp 0x100000 0x1fffff -> Lt
    0x100000 >= 0x1fffff -> false
    0x100000 <= 0x1fffff -> true
    0x100000 = 0x1fffff -> false
    0x100000 > 0x1fffff -> false
    0x100000 < 0x1fffff -> true
    0x100000 <> 0x1fffff -> true
    ascending 0x100000 0x1fffff -> Lt
    descending 0x100000 0x1fffff -> Gt

    clamp 0xffffe ~min:0xfffff ~max:0x100001 -> 0xfffff
    between 0xffffe ~low:0xfffff ~high:0x100001 -> false

    clamp 0xfffff ~min:0xfffff ~max:0x100001 -> 0xfffff
    between 0xfffff ~low:0xfffff ~high:0x100001 -> true

    clamp 0x100000 ~min:0xfffff ~max:0x100001 -> 0x100000
    between 0x100000 ~low:0xfffff ~high:0x100001 -> true

    clamp 0x100001 ~min:0xfffff ~max:0x100001 -> 0x100001
    between 0x100001 ~low:0xfffff ~high:0x100001 -> true

    clamp 0x100002 ~min:0xfffff ~max:0x100001 -> 0x100001
    between 0x100002 ~low:0xfffff ~high:0x100001 -> false
    |}]

let%expect_test "wraparound" =
  let open Printf in
  printf "max_value + 1 -> 0x%x\n" (to_int (max_value + (kv 1)));
  printf "min_value - 1 -> 0x%x\n" (to_int (min_value - (kv 1)));
  printf "max_value * 15 -> 0x%x\n" (to_int (max_value * (kv 15)));

  [%expect{|
    max_value + 1 -> 0x0
    min_value - 1 -> 0x1fffff
    max_value * 15 -> 0x1ffff1
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
        let t = of_uint (Uint.of_int x) in
        let u = to_uint t in
        let t' = of_uint u in
        printf "of_uint 0x%x -> to_uint 0x%x -> of_uint 0x%x -> 0x%x\n"
          x (to_int t) (Uint.to_int u) (to_int t');
        fn xs'
      end
  in
  fn [-1; 0; 42; 0x1f_ffff; 0x20_0000; 0x20_0001; max_int];

  [%expect{|
    of_int 0x7fffffffffffffff -> to_int 0x1fffff -> of_int 0x1fffff -> 0x1fffff
    of_uint 0x7fffffffffffffff -> to_uint 0x1fffff -> of_uint 0x1fffff -> 0x1fffff
    of_int 0x0 -> to_int 0x0 -> of_int 0x0 -> 0x0
    of_uint 0x0 -> to_uint 0x0 -> of_uint 0x0 -> 0x0
    of_int 0x2a -> to_int 0x2a -> of_int 0x2a -> 0x2a
    of_uint 0x2a -> to_uint 0x2a -> of_uint 0x2a -> 0x2a
    of_int 0x1fffff -> to_int 0x1fffff -> of_int 0x1fffff -> 0x1fffff
    of_uint 0x1fffff -> to_uint 0x1fffff -> of_uint 0x1fffff -> 0x1fffff
    of_int 0x200000 -> to_int 0x0 -> of_int 0x0 -> 0x0
    of_uint 0x200000 -> to_uint 0x0 -> of_uint 0x0 -> 0x0
    of_int 0x200001 -> to_int 0x1 -> of_int 0x1 -> 0x1
    of_uint 0x200001 -> to_uint 0x1 -> of_uint 0x1 -> 0x1
    of_int 0x3fffffffffffffff -> to_int 0x1fffff -> of_int 0x1fffff -> 0x1fffff
    of_uint 0x3fffffffffffffff -> to_uint 0x1fffff -> of_uint 0x1fffff -> 0x1fffff
    |}]
