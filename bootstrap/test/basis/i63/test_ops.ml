open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let x = kv 4 in
  let y = kv 3 in
  printf "%a + %a -> %a\n" pp x pp y pp (x + y);
  printf "%a - %a -> %a\n" pp x pp y pp (x - y);
  printf "%a * %a -> %a\n" pp x pp y pp (x * y);
  printf "%a / %a -> %a\n" pp x pp y pp (x / y);
  printf "%a %% %a -> %a\n" pp x pp y pp (x % y);
  printf "%a ** %a -> %a\n" pp x pp y pp (x ** y);
  printf "%a // %a -> %.2f\n" pp x pp y (x // y);
  let z = kv (-2) in
  printf "-(%a) -> %a\n" pp x pp (-x);
  printf "~-(%a) -> %a\n" pp x pp ~-x;
  printf "+(%a) -> %a\n" pp z pp (+z);
  printf "~+(%a) -> %a\n" pp z pp ~+z;
  printf "neg %a -> %a\n" pp x pp (neg x);
  printf "neg %a -> %a\n" pp z pp (neg z);
  printf "abs %a -> %a\n" pp x pp (abs x);
  printf "abs %a -> %a\n" pp z pp (abs z)

let _ = test ()
