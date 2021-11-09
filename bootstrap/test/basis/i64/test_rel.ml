open! Basis.Rudiments
open! Basis
open I64
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
  fn zero (of_string "0x8000_0000_0000_0000");
  printf "\n";
  fn zero (of_string "0xffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      xpp min xpp max xpp t xpp (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      xpp t xpp min xpp max (between ~low:min ~high:max t);
  end in
  fn2 (of_string "-2") (of_string "-1") (of_string "1");
  fn2 (of_string "-1") (of_string "-1") (of_string "1");
  fn2 (of_string "0") (of_string "-1") (of_string "1");
  fn2 (of_string "1") (of_string "-1") (of_string "1");
  fn2 (of_string "2") (of_string "-1") (of_string "1")

let _ = test ()
