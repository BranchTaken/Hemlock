open! Basis.Rudiments
open! Basis
open Z
open Format

let test () =
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let quotient = x / y in
        let remainder = x % y in
        printf "%a /,%% %a -> %a, %a\n"
          pp_x x pp_x y pp_x quotient pp_x remainder;
        assert (x = (y * quotient + remainder));
        assert (x >= y || remainder = x);
        test_pairs pairs'
      end
  in
  let pairs = [
    (* < 1 *)
    (of_string "0", of_string "1");
    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xfffe", of_string "0xffff");
    (of_string "0xffff_fffe", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_fffe", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xf_ffff_ffff_ffff_fffe", of_string "0xf_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (* Single-digit (base 2^32) divisor. *)
    (of_string "1", of_string "1");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "2");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "3");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "7");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff");

    (* Multi-digit (base 2^32) divisor. *)
    (of_string "0x1_0000_0000", of_string "0x1_0000_0000");
    (of_string "0x1_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0x2_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
