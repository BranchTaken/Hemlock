open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let fn x y = begin
    printf "cmp %a %a -> %a\n" pp x pp y Cmp.pp (cmp x y);
    printf "%a >= %a -> %b\n" pp x pp y (x >= y);
    printf "%a <= %a -> %b\n" pp x pp y (x <= y);
    printf "%a = %a -> %b\n" pp x pp y (x = y);
    printf "%a > %a -> %b\n" pp x pp y (x > y);
    printf "%a < %a -> %b\n" pp x pp y (x < y);
    printf "%a <> %a -> %b\n" pp x pp y (x <> y);
    printf "ascending %a %a -> %a\n" pp x pp y Cmp.pp (ascending x y);
    printf "descending %a %a -> %a\n" pp x pp y Cmp.pp (descending x y);
  end in
  fn ~-(kv 1) (kv 0);
  printf "\n";
  fn (kv 0) (kv 0);
  printf "\n";
  fn (kv 1) (kv 0);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp min pp max pp t pp (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp min pp max pp t (between ~low:min ~high:max t);
  end in
  fn2 ~-(kv 2) ~-(kv 1) (kv 1);
  fn2 ~-(kv 1) ~-(kv 1) (kv 1);
  fn2 (kv 0) ~-(kv 1) (kv 1);
  fn2 (kv 1) ~-(kv 1) (kv 1);
  fn2 (kv 2) ~-(kv 1) (kv 1)

let _ = test ()
