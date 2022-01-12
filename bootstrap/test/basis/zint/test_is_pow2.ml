open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "is_pow2 "
        |> fmt ~alt:true ~base:Fmt.Hex u
        |> Fmt.fmt " -> "
        |> Bool.fmt (is_pow2 u)
        |> Fmt.fmt "\n"
        |> ignore;
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us

let _ = test ()
