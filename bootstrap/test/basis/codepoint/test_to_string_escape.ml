open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let xpp_x xppf cp =
  Format.fprintf xppf "%a" Uns.xpp_x (extend_to_uns cp)

let utf8_xpp xppf t =
  Format.fprintf xppf "%s" (Utf8.escape t)

let test () =
  let rec fn i = begin
    match i with
    | 0x80L -> ()
    | _ -> begin
        let cp = trunc_of_uns i in
        let utf8 = Utf8.of_codepoint cp in
        printf "%a -> %a {|%s|} \"%a\"\n"
          Uns.xpp_x i
          xpp cp
          (if Uns.(i > 0x1fL && i < 0x7fL) then (to_string cp) else "�")
          utf8_xpp utf8;
        fn (Uns.succ i)
      end
  end in
  fn 0L

let _ = test ()
