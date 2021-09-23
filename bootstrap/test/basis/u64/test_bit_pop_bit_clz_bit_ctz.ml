open! Basis.Rudiments
open! Basis
open U64
open Format

let test () =
  printf "@[<h>";
  let rec test_u64s = function
    | [] -> ()
    | u :: us' -> begin
        printf "bit_{pop,clz,ctz} %a -> %Lu, %Lu, %Lu\n"
          pp_x u (bit_pop u) (bit_clz u) (bit_ctz u);
        test_u64s us'
      end
  in
  let us = [
    0L;
    1L;
    0x8000_0000_0000_0000L;
    0xffff_ffff_ffff_ffffL
  ] in
  test_u64s us;
  printf "@]"

let _ = test ()
