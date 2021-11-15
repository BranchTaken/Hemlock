open! Basis.Rudiments
open! Basis
open N

let test () =
  let rec test_strs = function
    | [] -> ()
    | s :: strs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "of_string "
        |> String.pp s
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~base:Fmt.Hex (of_string s)
        |> Fmt.fmt "\n"
        |> ignore;
        test_strs strs'
      end
  in
  let strs = [
    "0";
    "1";
    "9876543210";
    "9876543210_";
    "9";

    "57896044618658097711785492504343953926634992332820282019728792003956564819967";
    "115792089237316195423570985008687907853269984665640564039457584007913129639935";
    "231584178474632390847141970017375815706539969331281128078915168015826259279871";

    "0b0";
    "0b1";
    "0b10";
    "0b10_";
    "0b_1";
    ("0b_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111");

    "0x0";
    "0x1";
    "0xfedcba9876543210";
    "0xfedcba9876543210_";
    "0xf";
    "0x_f";
    "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";
  ] in
  test_strs strs

let _ = test ()
