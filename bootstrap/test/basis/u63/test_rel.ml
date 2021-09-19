open! Basis.Rudiments
open! Basis
open U63
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
  fn 0 0x4000_0000_0000_0000;
  printf "\n";
  fn 0 0x7fff_ffff_ffff_ffff;
  printf "\n";
  fn 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff;
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 0x3fff_ffff_ffff_fffe 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x3fff_ffff_ffff_ffff 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0001 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0002 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001

let _ = test ()
