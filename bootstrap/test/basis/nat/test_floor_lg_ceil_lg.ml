open! Basis.Rudiments
open! Basis
open Nat

let test () =
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "floor_lg,ceil_lg "
        |> fmt ~alt:true ~radix:Radix.Hex u
        |> Fmt.fmt " -> "
        |> fmt (floor_lg u)
        |> Fmt.fmt ", "
        |> fmt (ceil_lg u)
        |> Fmt.fmt "\n"
        |> ignore;
        test us'
      end
  in
  let us = [
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
