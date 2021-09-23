open! Basis.Rudiments
open! Basis
open I8
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_{pop,clz,ctz} %a -> %Lu, %Lu, %Lu\n"
          pp_x x (bit_pop x) (bit_clz x) (bit_ctz x);
        test xs'
      end
  in
  let xs = [
    kv (-0x80L);
    kv (-1L);
    kv 0L;
    kv 1L;
  ] in
  test xs;
  printf "@]"

let _ = test ()
