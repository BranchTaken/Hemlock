open! Basis.Rudiments
open! Basis
open U32
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_{pop,clz,ctz} %a -> %u, %u, %u\n"
          pp_x x (bit_pop x) (bit_clz x) (bit_ctz x);
        test xs'
      end
  in
  let xs = [
    kv 0;
    kv 1;
    kv 0x8000_0000;
    kv 0xffff_ffff
  ] in
  test xs;
  printf "@]"

let _ = test ()