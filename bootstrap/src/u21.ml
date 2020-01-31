(* Partial Rudiments. *)
module Usize = U63
module Isize = I63
open Rudiments_int
open Rudiments_functions

module T = struct
  type t = usize
  let num_bits = 21
end
include T
include Intnb.Make_u(T)

let to_isize t =
  Usize.to_isize t

let of_isize x =
  narrow_of_signed x

let of_isize_hlt x =
  let t = of_isize x in
  let x' = to_isize t in
  match Isize.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let kv x =
  narrow_of_unsigned x

let to_usize t =
  t

let of_usize x =
  narrow_of_unsigned x

let of_usize_hlt x =
  let t = of_usize x in
  let x' = to_usize t in
  match Usize.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let of_char c =
  Stdlib.Char.code c

let nul = 0x00
let soh = 0x01
let stx = 0x02
let etx = 0x03
let eot = 0x04
let enq = 0x05
let ack = 0x06
let bel = 0x07
let bs = of_char '\b'
let ht = of_char '\t'
let lf = of_char '\n'
let nl = of_char '\n'
let vt = 0x0b
let ff = 0x0c
let cr = of_char '\r'
let so = 0x0e
let si = 0x0f
let dle = 0x10
let dc1 = 0x11
let dc2 = 0x12
let dc3 = 0x13
let dc4 = 0x14
let nak = 0x15
let syn = 0x16
let etb = 0x17
let can = 0x18
let em = 0x19
let sub = 0x1a
let esc = 0x1b
let fs = 0x1c
let gs = 0x1d
let rs = 0x1e
let us = 0x1f
let del = 0x7f

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

  printf "num_bits=%a\n" Usize.pp num_bits;
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
  let fifteen = (kv 15) in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1u21 -> 0x000000u21
    min_value - 1u21 -> 0x1fffffu21
    max_value * 15u21 -> 0x1ffff1u21
    |}]

let%expect_test "conversion" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = isize_of_int x in
        let t = of_isize i in
        let i' = to_isize t in
        let t' = of_isize i' in
        printf "of_isize %a -> to_isize %a -> of_isize %a -> %a\n"
          Isize.pp_x i pp_x t Isize.pp_x i' pp_x t';
        let t = of_usize (Usize.of_isize i) in
        let u = to_usize t in
        let t' = of_usize u in
        printf "of_usize %a -> to_usize %a -> of_usize %a -> %a\n"
          Usize.pp_x x pp_x t Usize.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Usize.max_value; 0; 42; 0x1f_ffff; 0x20_0000; 0x20_0001;
      Usize.of_isize Isize.max_value];

  [%expect{|
    of_isize 0x7fffffffffffffffi -> to_isize 0x1fffffu21 -> of_isize 0x00000000001fffffi -> 0x1fffffu21
    of_usize 0x7fffffffffffffff -> to_usize 0x1fffffu21 -> of_usize 0x00000000001fffff -> 0x1fffffu21
    of_isize 0x0000000000000000i -> to_isize 0x000000u21 -> of_isize 0x0000000000000000i -> 0x000000u21
    of_usize 0x0000000000000000 -> to_usize 0x000000u21 -> of_usize 0x0000000000000000 -> 0x000000u21
    of_isize 0x000000000000002ai -> to_isize 0x00002au21 -> of_isize 0x000000000000002ai -> 0x00002au21
    of_usize 0x000000000000002a -> to_usize 0x00002au21 -> of_usize 0x000000000000002a -> 0x00002au21
    of_isize 0x00000000001fffffi -> to_isize 0x1fffffu21 -> of_isize 0x00000000001fffffi -> 0x1fffffu21
    of_usize 0x00000000001fffff -> to_usize 0x1fffffu21 -> of_usize 0x00000000001fffff -> 0x1fffffu21
    of_isize 0x0000000000200000i -> to_isize 0x000000u21 -> of_isize 0x0000000000000000i -> 0x000000u21
    of_usize 0x0000000000200000 -> to_usize 0x000000u21 -> of_usize 0x0000000000000000 -> 0x000000u21
    of_isize 0x0000000000200001i -> to_isize 0x000001u21 -> of_isize 0x0000000000000001i -> 0x000001u21
    of_usize 0x0000000000200001 -> to_usize 0x000001u21 -> of_usize 0x0000000000000001 -> 0x000001u21
    of_isize 0x3fffffffffffffffi -> to_isize 0x1fffffu21 -> of_isize 0x00000000001fffffi -> 0x1fffffu21
    of_usize 0x3fffffffffffffff -> to_usize 0x1fffffu21 -> of_usize 0x00000000001fffff -> 0x1fffffu21
    |}]
