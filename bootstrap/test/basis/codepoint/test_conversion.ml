open! Basis.Rudiments
open! Basis
open Codepoint

let pp_uns_x u formatter =
  formatter |> Uns.fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex u

let pp_x cp formatter =
  formatter |> pp_uns_x (extend_to_uns cp)

let test () =
  let rec fn xs formatter =
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let i = x in
        let t = trunc_of_uns (Uns.bits_of_sint i) in
        let u = extend_to_uns t in
        let t' = trunc_of_uns u in
        formatter
        |> Fmt.fmt "trunc_of_uns "
        |> pp_uns_x x
        |> Fmt.fmt " -> extend_to_uns "
        |> pp_x t
        |> Fmt.fmt " -> trunc_of_uns "
        |> pp_uns_x u
        |> Fmt.fmt " -> "
        |> pp_x t'
        |> Fmt.fmt "\n"
        |> fn xs'
      end
  in
  File.Fmt.stdout
  |> fn [Uns.max_value; 0L; 42L; 0xd800L; 0xdfffL; 0x10_ffffL; 0x11_0000L; 0x20_0000L; 0x20_0001L]
  |> ignore

let _ = test ()
