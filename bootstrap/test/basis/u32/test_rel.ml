open! Basis.Rudiments
open! Basis
open U32
open Format

let test () =
  let fn x y = begin
    printf "cmp %a %a -> %a\n" xpp_x x xpp_x y Cmp.xpp (cmp x y);
    printf "%a >= %a -> %b\n" xpp_x x xpp_x y (x >= y);
    printf "%a <= %a -> %b\n" xpp_x x xpp_x y (x <= y);
    printf "%a = %a -> %b\n" xpp_x x xpp_x y (x = y);
    printf "%a > %a -> %b\n" xpp_x x xpp_x y (x > y);
    printf "%a < %a -> %b\n" xpp_x x xpp_x y (x < y);
    printf "%a <> %a -> %b\n" xpp_x x xpp_x y (x <> y);
    printf "ascending %a %a -> %a\n" xpp_x x xpp_x y Cmp.xpp (ascending x y);
    printf "descending %a %a -> %a\n" xpp_x x xpp_x y Cmp.xpp (descending x y);
  end in
  fn (kv 0L) (kv 0x8000_0000L);
  printf "\n";
  fn (kv 0L) (kv 0xffff_ffffL);
  printf "\n";
  fn (kv 0x8000_0000L) (kv 0xffff_ffffL);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      xpp_x min xpp_x max xpp_x t xpp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      xpp_x min xpp_x max xpp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv 0x7fff_fffeL) (kv 0x7fff_ffffL) (kv 0x8000_0001L);
  fn2 (kv 0x7fff_ffffL) (kv 0x7fff_ffffL) (kv 0x8000_0001L);
  fn2 (kv 0x8000_0000L) (kv 0x7fff_ffffL) (kv 0x8000_0001L);
  fn2 (kv 0x8000_0001L) (kv 0x7fff_ffffL) (kv 0x8000_0001L);
  fn2 (kv 0x8000_0002L) (kv 0x7fff_ffffL) (kv 0x8000_0001L)

let _ = test ()
