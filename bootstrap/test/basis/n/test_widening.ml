open! Basis.Rudiments
open! Basis
open N
open Format

let test () =
  let u64_max = of_u64 U64.max_value in
  let fifteen = of_string "15" in
  printf "u64_max -> %a\n" pp_x u64_max;

  let r = (u64_max + one) in
  printf "u64_max + %a -> %a %a\n" pp one pp_x r pp r;

  (* This underflow unintuitively results in two words rather than one, because it doesn't make
   * sense to trim leading "sign" bits for an arbitrary-precision unsigned result. *)
  let r = (zero - one) in
  printf "%a - %a -> %a %a\n" pp zero pp one pp_x r pp r;

  let r = (u64_max * fifteen) in
  printf "u64_max * %a -> %a %a\n" pp fifteen pp_x r pp r

let _ = test ()
