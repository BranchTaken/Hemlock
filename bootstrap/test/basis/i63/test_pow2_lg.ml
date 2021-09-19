open! Basis.Rudiments
open! Basis
open I63
open Format

let test () =
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "is_pow2 %a -> %b\n" pp_x x (is_pow2 x);
        if x > kv 0 then begin
          printf "floor_pow2 %a -> %a\n" pp_x x pp_x (floor_pow2 x);
          printf "ceil_pow2 %a -> %a\n" pp_x x pp_x (ceil_pow2 x);
          printf "floor_lg %a -> %a\n" pp_x x pp (floor_lg x);
          printf "ceil_lg %a -> %a\n" pp_x x pp (ceil_lg x)
        end;
        fn xs'
      end
  end in
  fn [kv 0; kv 1; kv 2; kv 3; kv 4; kv 0xf0; max_value]

let _ = test ()
