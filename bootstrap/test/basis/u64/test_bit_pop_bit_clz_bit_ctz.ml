open! Basis.Rudiments
open! Basis
open U64

let test () =
  let rec test_u64s = function
    | [] -> ()
    | u :: us' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_{pop,clz,ctz} "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex u
        |> Fmt.fmt " -> "
        |> Uns.pp (bit_pop u)
        |> Fmt.fmt ", "
        |> Uns.pp (bit_clz u)
        |> Fmt.fmt ", "
        |> Uns.pp (bit_ctz u)
        |> Fmt.fmt "\n"
        |> ignore;
        test_u64s us'
      end
  in
  let us = [
    0L;
    1L;
    0x8000_0000_0000_0000L;
    0xffff_ffff_ffff_ffffL
  ] in
  test_u64s us

let _ = test ()
