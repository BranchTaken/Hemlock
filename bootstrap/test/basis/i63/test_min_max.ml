open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let fn x y = begin
    printf "min %a %a -> %a\n" pp x pp y pp (min x y);
    printf "max %a %a -> %a\n" pp x pp y pp (max x y);
  end in
  fn ~-(kv 1) (kv 0);
  printf "\n";
  fn (kv 0) (kv 0);
  printf "\n";
  fn (kv 1) (kv 0)

let _ = test ()
