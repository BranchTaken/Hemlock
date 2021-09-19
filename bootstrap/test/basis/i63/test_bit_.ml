open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let x = kv 0b0011 in
  let y = kv 0b0101 in
  printf "bit_and %a %a -> %a\n" pp_x x pp_x y pp_x (bit_and x y);
  printf "bit_or %a %a -> %a\n" pp_x x pp_x y pp_x (bit_or x y);
  printf "bit_xor %a %a -> %a\n" pp_x x pp_x y pp_x (bit_xor x y);

  let x = kv 0b10 in
  printf "bit_not %a -> %a\n" pp_x x pp_x (bit_not x);

  let x = kv 0xff in
  let s = 4 in
  printf "bit_sl %a %a -> %a\n" Uns.pp s pp_x x pp_x (bit_sl ~shift:s x);

  let x = kv (-1) in
  let s = 4 in
  printf "bit_usr %a %a -> %a\n" Uns.pp s pp_x x pp_x (bit_usr ~shift:s x);
  printf "bit_ssr %a %a -> %a\n" Uns.pp s pp_x x pp_x (bit_ssr ~shift:s x);

  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_pop %a -> %a\n" pp_x x Uns.pp (bit_pop x);
        printf "bit_clz %a -> %a\n" pp_x x Uns.pp (bit_clz x);
        printf "bit_ctz %a -> %a\n" pp_x x Uns.pp (bit_ctz x);
        fn xs'
      end
  end in
  fn [kv (-1); kv 0; kv 1; min_value; max_value; kv 0xf73100]

let _ = test ()
