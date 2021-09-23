open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let pp_x ppf cp =
  Format.fprintf ppf "%a" Uns.pp_x (to_uns cp)

let utf8_pp ppf t =
  Format.fprintf ppf "%s" (Utf8.escape t)

let test () =
  let rec fn i = begin
    match i with
    | 0x80L -> ()
    | _ -> begin
        let cp = of_uns i in
        let utf8 = Utf8.of_codepoint cp in
        printf "%a -> %a {|%s|} \"%a\"\n"
          Uns.pp_x i
          pp cp
          (if Uns.(i > 0x1fL && i < 0x7fL) then (to_string cp) else "�")
          utf8_pp utf8;
        fn (Uns.succ i)
      end
  end in
  fn 0L

let _ = test ()
