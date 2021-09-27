open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let pp_x ppf cp =
  Format.fprintf ppf "%a" Uns.pp_x (to_uns cp)

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
  fn (kv 0) (kv 0x10_0000);
  printf "\n";
  fn (kv 0) (kv 0x10_ffff);
  printf "\n";
  fn (kv 0x10_0000) (kv 0x10_ffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv 0x0f_fffe) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x0f_ffff) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0000) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0001) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0002) (kv 0x0f_ffff) (kv 0x10_0001)

let _ = test ()