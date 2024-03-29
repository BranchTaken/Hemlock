open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin

        File.Fmt.stdout
        |> fmt ~alt:true ~radix:Radix.Hex x
        |> Fmt.fmt " +,- "
        |> fmt ~alt:true ~radix:Radix.Hex y
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~radix:Radix.Hex (x + y)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~radix:Radix.Hex (x - y)
        |> Fmt.fmt "\n"
        |> ignore;
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");

    (of_string "0", of_string "1");
    (of_string "1", of_string "0");

    (of_string "0", of_string "-1");
    (of_string "-1", of_string "0");

    (of_string "1", of_string "-1");
    (of_string "-1", of_string "1");

    (of_string "1", of_string "0x3fff_ffff_ffff_ffff");
    (of_string "0x3fff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0x7fff_ffff_ffff_ffff");
    (of_string "0x7fff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string
        "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");

    (of_string "1", of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");

    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
  ] in
  test_pairs pairs

let _ = test ()
