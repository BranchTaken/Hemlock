open! Basis.Rudiments
open! Basis
open I256
open Format

let test () =
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let quotient = x / y in
        let remainder = x % y in
        printf "%a /,%% %a -> %a, %a\n"
          xpp x xpp y xpp quotient xpp remainder;
        assert (x = (y * quotient + remainder));
        test_pairs pairs'
      end
  in
  let pairs = [
    (* < 1 *)
    (of_string "0", of_string "1");

    (of_string "1", of_string "1");
    (of_string "-1", of_string "1");
    (of_string "1", of_string "-1");
    (of_string "-1", of_string "-1");

    (of_string "7", of_string "3");
    (of_string "-7", of_string "3");
    (of_string "7", of_string "-3");
    (of_string "-7", of_string "-3");

    (of_string "1", max_value);
    (max_value, of_string "1");

    (of_string "1", min_value);
    (min_value, of_string "1");

    (of_string "0xfffe", of_string "0xffff");
    (of_string "0xffff_fffe", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_fffe", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xf_ffff_ffff_ffff_fffe", of_string "0xf_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (max_value - one, max_value);

    (* Single-digit (base 2^32) divisor. *)
    (max_value, of_string "1");
    (max_value, of_string "2");
    (max_value, of_string "3");
    (max_value, of_string "7");
    (max_value, of_string "0xffff");
    (max_value, of_string "0xffff_ffff");

    (min_value, of_string "1");
    (min_value, of_string "-1"); (* Surprising sign due to overflow. *)
    (min_value, of_string "2");
    (min_value, of_string "-2");
    (min_value, of_string "3");
    (min_value, of_string "7");
    (min_value, of_string "0xffff");
    (min_value, of_string "0xffff_ffff");

    (* Multi-digit (base 2^32) divisor. *)
    (of_string "0x1_0000_0000", of_string "0x1_0000_0000");
    (of_string "0x1_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0x2_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (max_value, max_value);
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff");
    (max_value, of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
