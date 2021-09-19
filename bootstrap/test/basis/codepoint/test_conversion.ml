open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let pp_x ppf cp =
  Format.fprintf ppf "%a" Uns.pp_x (to_uns cp)

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = sint_of_int x in
        let t = of_uns (Uns.of_sint i) in
        let u = to_uns t in
        let t' = of_uns u in
        printf "of_uns %a -> to_uns %a -> of_uns %a -> %a\n"
          Uns.pp_x x pp_x t Uns.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Uns.max_value; 0; 42; 0xd800; 0xdfff; 0x10_ffff; 0x11_0000; 0x20_0000; 0x20_0001]

let _ = test ()
