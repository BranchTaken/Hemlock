open! Basis.Rudiments
open! Basis
open Nat

let test () =
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "floor_pow2,ceil_pow2 "
        |> fmt ~alt:true ~radix:Radix.Hex u
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~radix:Radix.Hex (floor_pow2 u)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~radix:Radix.Hex (ceil_pow2 u)
        |> Fmt.fmt "\n"
        |> ignore;
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";

    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff";

    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";

    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us

let _ = test ()
