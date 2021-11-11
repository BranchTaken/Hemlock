open! Basis.Rudiments
open! Basis
open Codepoint

let pp_uns_x u formatter =
  formatter |> Uns.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex u

let pp_x cp formatter =
  formatter |> pp_uns_x (extend_to_uns cp)

let utf8_pp t formatter =
  formatter |> Fmt.fmt (Utf8.escape t)

let test () =
  let rec fn i = begin
    match i with
    | 0x80L -> ()
    | _ -> begin
        let cp = trunc_of_uns i in
        let utf8 = Utf8.of_codepoint cp in
        File.Fmt.stdout
        |> pp_uns_x i
        |> Fmt.fmt " -> "
        |> pp cp
        |> Fmt.fmt " `_`"
        |> Fmt.fmt (if Uns.(i > 0x1fL && i < 0x7fL) then (to_string cp) else "�")
        |> Fmt.fmt "`_` \""
        |> utf8_pp utf8
        |> Fmt.fmt "\"\n"
        |> ignore;
        fn (Uns.succ i)
      end
  end in
  fn 0L

let _ = test ()
