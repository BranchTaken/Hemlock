open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec test_strs = function
    | [] -> ()
    | s :: strs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "of_string "
        |> String.pp s
        |> Fmt.fmt " -> "
        |> pp (of_string s)
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (of_string s)
        |> Fmt.fmt "\n"
        |> ignore;
        test_strs strs'
      end
  in
  let strs = [
    "-1";
    "0";
    "1";
    "+1";
    "9876543210";
    "9876543210_";
    "9i256";
    "9_i256";
    "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    "0b0";
    "0b1";
    "0b10";
    "0b10_";
    "0b1i256";
    "0b_1_i256";
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
    "0xfi256";
    "0x_f_i256";
    "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";
  ] in
  test_strs strs

let _ = test ()
