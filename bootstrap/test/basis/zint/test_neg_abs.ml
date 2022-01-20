open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        let fn u = begin
          File.Fmt.stdout
          |> Fmt.fmt "neg,abs "
          |> fmt ~alt:true ~radix:Radix.Hex u
          |> Fmt.fmt " -> "
          |> fmt ~alt:true ~radix:Radix.Hex (neg u)
          |> Fmt.fmt ", "
          |> fmt ~alt:true ~radix:Radix.Hex (abs u)
          |> Fmt.fmt "\n"
          |> ignore
        end in
        fn u;
        fn (neg u);
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test us

let _ = test ()
