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

let pp ppf t =
  Format.fprintf ppf "%uu8" (to_int t)

let pp_x ppf t =
  Format.fprintf ppf "0x%02xu8" (to_int t)

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
  fn [kv 0; kv 1; kv 42; kv 255];
  printf "@]";

  [%expect{|
0u8 0x00u8
1u8 0x01u8
42u8 0x2au8
255u8 0xffu8
    |}]

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" Uint.pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=8
    min_value=0x00u8
    max_value=0xffu8
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
  fn (kv 0) (kv 0x80);
  printf "\n";
  fn (kv 0) (kv 0xff);
  printf "\n";
  fn (kv 0x80) (kv 0xff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp %a ~min:%a ~max:%a -> %a\n"
      pp_x t pp_x min pp_x max pp_x (clamp t ~min ~max);
    printf "between %a ~low:%a ~high:%a -> %b\n"
      pp_x t pp_x min pp_x max (between t ~low:min ~high:max);
  end in
  fn2 (kv 0x7e) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x7f) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x80) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x81) (kv 0x7f) (kv 0x81);
  fn2 (kv 0x82) (kv 0x7f) (kv 0x81);

  [%expect{|
    cmp 0x00u8 0x80u8 -> Lt
    0x00u8 >= 0x80u8 -> false
    0x00u8 <= 0x80u8 -> true
    0x00u8 = 0x80u8 -> false
    0x00u8 > 0x80u8 -> false
    0x00u8 < 0x80u8 -> true
    0x00u8 <> 0x80u8 -> true
    ascending 0x00u8 0x80u8 -> Lt
    descending 0x00u8 0x80u8 -> Gt

    cmp 0x00u8 0xffu8 -> Lt
    0x00u8 >= 0xffu8 -> false
    0x00u8 <= 0xffu8 -> true
    0x00u8 = 0xffu8 -> false
    0x00u8 > 0xffu8 -> false
    0x00u8 < 0xffu8 -> true
    0x00u8 <> 0xffu8 -> true
    ascending 0x00u8 0xffu8 -> Lt
    descending 0x00u8 0xffu8 -> Gt

    cmp 0x80u8 0xffu8 -> Lt
    0x80u8 >= 0xffu8 -> false
    0x80u8 <= 0xffu8 -> true
    0x80u8 = 0xffu8 -> false
    0x80u8 > 0xffu8 -> false
    0x80u8 < 0xffu8 -> true
    0x80u8 <> 0xffu8 -> true
    ascending 0x80u8 0xffu8 -> Lt
    descending 0x80u8 0xffu8 -> Gt

    clamp 0x7eu8 ~min:0x7fu8 ~max:0x81u8 -> 0x7fu8
    between 0x7eu8 ~low:0x7fu8 ~high:0x81u8 -> false

    clamp 0x7fu8 ~min:0x7fu8 ~max:0x81u8 -> 0x7fu8
    between 0x7fu8 ~low:0x7fu8 ~high:0x81u8 -> true

    clamp 0x80u8 ~min:0x7fu8 ~max:0x81u8 -> 0x80u8
    between 0x80u8 ~low:0x7fu8 ~high:0x81u8 -> true

    clamp 0x81u8 ~min:0x7fu8 ~max:0x81u8 -> 0x81u8
    between 0x81u8 ~low:0x7fu8 ~high:0x81u8 -> true

    clamp 0x82u8 ~min:0x7fu8 ~max:0x81u8 -> 0x81u8
    between 0x82u8 ~low:0x7fu8 ~high:0x81u8 -> false
    |}]

let%expect_test "wraparound" =
  let open Format in
  printf "max_value + 1 -> %a\n" pp_x (max_value + (kv 1));
  printf "min_value - 1 -> %a\n" pp_x (min_value - (kv 1));
  printf "max_value * 15 -> %a\n" pp_x (max_value * (kv 15));

  [%expect{|
    max_value + 1 -> 0x00u8
    min_value - 1 -> 0xffu8
    max_value * 15 -> 0xf1u8
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
        let t = of_uint (Uint.kv x) in
        let u = to_uint t in
        let t' = of_uint u in
        printf "of_uint 0x%x -> to_uint %a -> of_uint %a -> %a\n"
          x pp_x t Uint.pp_x u pp_x t';

        let c = U21.of_uint (Uint.of_int x) in
        let t = of_codepoint c in
        let c' = to_codepoint t in
        let t' = of_codepoint c' in
        printf ("Codepoint.of_uint 0x%x -> of_codepoint %a -> " ^^
          "to_codepoint %a -> of_codepoint %a -> %a\n") x
          Codepoint.pp_x c pp_x t Codepoint.pp_x c' pp_x t';

        fn xs'
      end
  in
  fn [-1; 0; 42; 127; 128; 255; 256; 257; max_int];

  [%expect{|
    of_int 0x7fffffffffffffff -> to_int 0xffu8 -> of_int 0xff -> 0xffu8
    of_uint 0x7fffffffffffffff -> to_uint 0xffu8 -> of_uint 0x00000000000000ff -> 0xffu8
    Codepoint.of_uint 0x7fffffffffffffff -> of_codepoint 0x1fffffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    of_int 0x0 -> to_int 0x00u8 -> of_int 0x0 -> 0x00u8
    of_uint 0x0 -> to_uint 0x00u8 -> of_uint 0x0000000000000000 -> 0x00u8
    Codepoint.of_uint 0x0 -> of_codepoint 0x000000u21 -> to_codepoint 0x00u8 -> of_codepoint 0x000000u21 -> 0x00u8
    of_int 0x2a -> to_int 0x2au8 -> of_int 0x2a -> 0x2au8
    of_uint 0x2a -> to_uint 0x2au8 -> of_uint 0x000000000000002a -> 0x2au8
    Codepoint.of_uint 0x2a -> of_codepoint 0x00002au21 -> to_codepoint 0x2au8 -> of_codepoint 0x00002au21 -> 0x2au8
    of_int 0x7f -> to_int 0x7fu8 -> of_int 0x7f -> 0x7fu8
    of_uint 0x7f -> to_uint 0x7fu8 -> of_uint 0x000000000000007f -> 0x7fu8
    Codepoint.of_uint 0x7f -> of_codepoint 0x00007fu21 -> to_codepoint 0x7fu8 -> of_codepoint 0x00007fu21 -> 0x7fu8
    of_int 0x80 -> to_int 0x80u8 -> of_int 0x80 -> 0x80u8
    of_uint 0x80 -> to_uint 0x80u8 -> of_uint 0x0000000000000080 -> 0x80u8
    Codepoint.of_uint 0x80 -> of_codepoint 0x000080u21 -> to_codepoint 0x80u8 -> of_codepoint 0x000080u21 -> 0x80u8
    of_int 0xff -> to_int 0xffu8 -> of_int 0xff -> 0xffu8
    of_uint 0xff -> to_uint 0xffu8 -> of_uint 0x00000000000000ff -> 0xffu8
    Codepoint.of_uint 0xff -> of_codepoint 0x0000ffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    of_int 0x100 -> to_int 0x00u8 -> of_int 0x0 -> 0x00u8
    of_uint 0x100 -> to_uint 0x00u8 -> of_uint 0x0000000000000000 -> 0x00u8
    Codepoint.of_uint 0x100 -> of_codepoint 0x000100u21 -> to_codepoint 0x00u8 -> of_codepoint 0x000000u21 -> 0x00u8
    of_int 0x101 -> to_int 0x01u8 -> of_int 0x1 -> 0x01u8
    of_uint 0x101 -> to_uint 0x01u8 -> of_uint 0x0000000000000001 -> 0x01u8
    Codepoint.of_uint 0x101 -> of_codepoint 0x000101u21 -> to_codepoint 0x01u8 -> of_codepoint 0x000001u21 -> 0x01u8
    of_int 0x3fffffffffffffff -> to_int 0xffu8 -> of_int 0xff -> 0xffu8
    of_uint 0x3fffffffffffffff -> to_uint 0xffu8 -> of_uint 0x00000000000000ff -> 0xffu8
    Codepoint.of_uint 0x3fffffffffffffff -> of_codepoint 0x1fffffu21 -> to_codepoint 0xffu8 -> of_codepoint 0x0000ffu21 -> 0xffu8
    |}]
