open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        let fn u = begin
          File.Fmt.stdout
          |> Fmt.fmt "to_i64,to_sint "
          |> fmt ~alt:true ~base:Fmt.Hex ~pretty:true u
          |> Fmt.fmt " -> "
          |> I64.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true (to_i64 u)
          |> Fmt.fmt ", "
          |> Sint.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true (to_sint u)
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
