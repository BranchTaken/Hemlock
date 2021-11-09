open! Basis.Rudiments
open! Basis
open I8
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
  fn (kv 0L) (kv (-0x80L));
  printf "\n";
  fn (kv 0L) (kv (-1L));
  printf "\n";
  fn (kv (-0x80L)) (kv (-1L));
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      xpp_x min xpp_x max xpp_x t xpp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      xpp_x min xpp_x max xpp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv (-2L)) (kv (-1L)) (kv 0L);
  fn2 (kv (-1L)) (kv (-1L)) (kv 0L);
  fn2 (kv 0L) (kv (-1L)) (kv 0L);
  fn2 (kv 1L) (kv (-1L)) (kv 0L)

let _ = test ()
