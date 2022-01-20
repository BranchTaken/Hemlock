open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_length, bit_{pop,clz,ctz} "
        |> fmt ~alt:true ~radix:Radix.Hex x
        |> Fmt.fmt " -> "
        |> Uns.fmt (bit_length x)
        |> Fmt.fmt ", "
        |> Uns.fmt (bit_pop x)
        |> Fmt.fmt ", "
        |> Uns.fmt (bit_clz x)
        |> Fmt.fmt ", "
        |> Uns.fmt (bit_ctz x)
        |> Fmt.fmt "\n"
        |> ignore;
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test xs

let _ = test ()
