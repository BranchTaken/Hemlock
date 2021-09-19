open! Basis.Rudiments
open! Basis
open U64
open Format

let test () =
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
  fn zero (of_string "0x8000_0000_0000_0000");
  printf "\n";
  fn zero (of_string "0xffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (of_string "0x7fff_ffff_ffff_fffe") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x7fff_ffff_ffff_ffff") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0001") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0002") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001")

let _ = test ()
