open! Basis.Rudiments
open! Basis
open I64
open Format

let test () =
  printf "@[<h>";
  let rec test_u64s = function
    | [] -> ()
    | u :: us' -> begin
        printf "bit_{pop,clz,ctz} %a -> %Lu, %Lu, %Lu\n"
          xpp_x u (bit_pop u) (bit_clz u) (bit_ctz u);
        test_u64s us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test_u64s us;
  printf "@]"

let _ = test ()
