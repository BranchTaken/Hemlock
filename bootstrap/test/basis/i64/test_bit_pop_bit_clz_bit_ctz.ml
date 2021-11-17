open! Basis.Rudiments
open! Basis
open I64

let test () =
  let rec test_i64s = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_{pop,clz,ctz} "
        |> fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> Uns.pp (bit_pop x)
        |> Fmt.fmt ", "
        |> Uns.pp (bit_clz x)
        |> Fmt.fmt ", "
        |> Uns.pp (bit_ctz x)
        |> Fmt.fmt "\n"
        |> ignore;
        test_i64s xs'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test_i64s us

let _ = test ()
