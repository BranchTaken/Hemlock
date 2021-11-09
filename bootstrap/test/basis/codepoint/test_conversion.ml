open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let xpp_x xppf cp =
  Format.fprintf xppf "%a" Uns.xpp_x (extend_to_uns cp)

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = x in
        let t = trunc_of_uns (Uns.bits_of_sint i) in
        let u = extend_to_uns t in
        let t' = trunc_of_uns u in
        printf "trunc_of_uns %a -> extend_to_uns %a -> trunc_of_uns %a -> %a\n"
          Uns.xpp_x x xpp_x t Uns.xpp_x u xpp_x t';
        fn xs'
      end
  in
  fn [Uns.max_value; 0L; 42L; 0xd800L; 0xdfffL; 0x10_ffffL; 0x11_0000L; 0x20_0000L; 0x20_0001L]

let _ = test ()
