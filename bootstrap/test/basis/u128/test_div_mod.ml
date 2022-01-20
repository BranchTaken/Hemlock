open! Basis.Rudiments
open! Basis
open U128

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let quotient = x / y in
        let remainder = x % y in
        File.Fmt.stdout
        |> fmt ~alt:true ~zpad:true ~width:32L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt " /,% "
        |> fmt ~alt:true ~zpad:true ~width:32L ~radix:Radix.Hex ~pretty:true y
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:32L ~radix:Radix.Hex ~pretty:true quotient
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:32L ~radix:Radix.Hex ~pretty:true remainder
        |> Fmt.fmt "\n"
        |> ignore;
        assert (x = (y * quotient + remainder));
        test_pairs pairs'
      end
  in
  let pairs = [
    (* < 1 *)
    (of_string "0", of_string "1");
    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (of_string "0xfffe", of_string "0xffff");
    (of_string "0xffff_fffe", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_fffe", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xf_ffff_ffff_ffff_fffe", of_string "0xf_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (* Single-digit (base 2^32) divisor. *)
    (of_string "1", of_string "1");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "2");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "3");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "7");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "0xffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff");

    (* Multi-digit (base 2^32) divisor. *)
    (of_string "0x1_0000_0000", of_string "0x1_0000_0000");
    (of_string "0x1_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0x2_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs

let _ = test ()
