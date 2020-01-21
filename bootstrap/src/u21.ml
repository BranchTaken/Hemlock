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

let pp ppf t =
  Format.fprintf ppf "%uu21" (to_int t)

let pp_x ppf t =
  Format.fprintf ppf "0x%06xu21" (to_int t)

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
  fn [kv 0; kv 1; kv 42; kv 0x7fffff];
  printf "@]";

  [%expect{|
    0u21 0x000000u21
    1u21 0x000001u21
    42u21 0x00002au21
    2097151u21 0x1fffffu21
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Uint.pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=21
    min_value=0x000000u21
    max_value=0x1fffffu21
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
  fn (kv 0) (kv 0x10_0000);
  printf "\n";
  fn (kv 0) (kv 0x1f_ffff);
  printf "\n";
  fn (kv 0x10_0000) (kv 0x1f_ffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp %a ~min:%a ~max:%a -> %a\n"
      pp_x t pp_x min pp_x max pp_x (clamp t ~min ~max);
    printf "between %a ~low:%a ~high:%a -> %b\n"
      pp_x t pp_x min pp_x max (between t ~low:min ~high:max);
  end in
  fn2 (kv 0x0f_fffe) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x0f_ffff) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0000) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0001) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0002) (kv 0x0f_ffff) (kv 0x10_0001);

  [%expect{|
    cmp 0x000000u21 0x100000u21 -> Lt
    0x000000u21 >= 0x100000u21 -> false
    0x000000u21 <= 0x100000u21 -> true
    0x000000u21 = 0x100000u21 -> false
    0x000000u21 > 0x100000u21 -> false
    0x000000u21 < 0x100000u21 -> true
    0x000000u21 <> 0x100000u21 -> true
    ascending 0x000000u21 0x100000u21 -> Lt
    descending 0x000000u21 0x100000u21 -> Gt

    cmp 0x000000u21 0x1fffffu21 -> Lt
    0x000000u21 >= 0x1fffffu21 -> false
    0x000000u21 <= 0x1fffffu21 -> true
    0x000000u21 = 0x1fffffu21 -> false
    0x000000u21 > 0x1fffffu21 -> false
    0x000000u21 < 0x1fffffu21 -> true
    0x000000u21 <> 0x1fffffu21 -> true
    ascending 0x000000u21 0x1fffffu21 -> Lt
    descending 0x000000u21 0x1fffffu21 -> Gt

    cmp 0x100000u21 0x1fffffu21 -> Lt
    0x100000u21 >= 0x1fffffu21 -> false
    0x100000u21 <= 0x1fffffu21 -> true
    0x100000u21 = 0x1fffffu21 -> false
    0x100000u21 > 0x1fffffu21 -> false
    0x100000u21 < 0x1fffffu21 -> true
    0x100000u21 <> 0x1fffffu21 -> true
    ascending 0x100000u21 0x1fffffu21 -> Lt
    descending 0x100000u21 0x1fffffu21 -> Gt

    clamp 0x0ffffeu21 ~min:0x0fffffu21 ~max:0x100001u21 -> 0x0fffffu21
    between 0x0ffffeu21 ~low:0x0fffffu21 ~high:0x100001u21 -> false

    clamp 0x0fffffu21 ~min:0x0fffffu21 ~max:0x100001u21 -> 0x0fffffu21
    between 0x0fffffu21 ~low:0x0fffffu21 ~high:0x100001u21 -> true

    clamp 0x100000u21 ~min:0x0fffffu21 ~max:0x100001u21 -> 0x100000u21
    between 0x100000u21 ~low:0x0fffffu21 ~high:0x100001u21 -> true

    clamp 0x100001u21 ~min:0x0fffffu21 ~max:0x100001u21 -> 0x100001u21
    between 0x100001u21 ~low:0x0fffffu21 ~high:0x100001u21 -> true

    clamp 0x100002u21 ~min:0x0fffffu21 ~max:0x100001u21 -> 0x100001u21
    between 0x100002u21 ~low:0x0fffffu21 ~high:0x100001u21 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  printf "max_value + 1 -> %a\n" pp_x (max_value + (kv 1));
  printf "min_value - 1 -> %a\n" pp_x (min_value - (kv 1));
  printf "max_value * 15 -> %a\n" pp_x (max_value * (kv 15));

  [%expect{|
    max_value + 1 -> 0x000000u21
    min_value - 1 -> 0x1fffffu21
    max_value * 15 -> 0x1ffff1u21
    |}]

let%expect_test "conversion" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let t = of_int x in
        let i = to_int t in
        let t' = of_int i in
        printf "of_int 0x%x -> to_int %a -> of_int 0x%x -> %a\n"
          x pp_x t i pp_x t';
        let t = of_uint (Uint.of_int x) in
        let u = to_uint t in
        let t' = of_uint u in
        printf "of_uint 0x%x -> to_uint %a -> of_uint %a -> %a\n"
          x pp_x t Uint.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [-1; 0; 42; 0x1f_ffff; 0x20_0000; 0x20_0001; max_int];

  [%expect{|
    of_int 0x7fffffffffffffff -> to_int 0x1fffffu21 -> of_int 0x1fffff -> 0x1fffffu21
    of_uint 0x7fffffffffffffff -> to_uint 0x1fffffu21 -> of_uint 0x00000000001fffff -> 0x1fffffu21
    of_int 0x0 -> to_int 0x000000u21 -> of_int 0x0 -> 0x000000u21
    of_uint 0x0 -> to_uint 0x000000u21 -> of_uint 0x0000000000000000 -> 0x000000u21
    of_int 0x2a -> to_int 0x00002au21 -> of_int 0x2a -> 0x00002au21
    of_uint 0x2a -> to_uint 0x00002au21 -> of_uint 0x000000000000002a -> 0x00002au21
    of_int 0x1fffff -> to_int 0x1fffffu21 -> of_int 0x1fffff -> 0x1fffffu21
    of_uint 0x1fffff -> to_uint 0x1fffffu21 -> of_uint 0x00000000001fffff -> 0x1fffffu21
    of_int 0x200000 -> to_int 0x000000u21 -> of_int 0x0 -> 0x000000u21
    of_uint 0x200000 -> to_uint 0x000000u21 -> of_uint 0x0000000000000000 -> 0x000000u21
    of_int 0x200001 -> to_int 0x000001u21 -> of_int 0x1 -> 0x000001u21
    of_uint 0x200001 -> to_uint 0x000001u21 -> of_uint 0x0000000000000001 -> 0x000001u21
    of_int 0x3fffffffffffffff -> to_int 0x1fffffu21 -> of_int 0x1fffff -> 0x1fffffu21
    of_uint 0x3fffffffffffffff -> to_uint 0x1fffffu21 -> of_uint 0x00000000001fffff -> 0x1fffffu21
    |}]
