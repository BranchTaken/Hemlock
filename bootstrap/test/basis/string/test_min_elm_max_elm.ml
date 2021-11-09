open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_elm s ~cmp = begin
    printf "min_elm %a -> %s\n" xpp s (match min_elm s ~cmp with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    );
    printf "max_elm %a -> %s\n" xpp s (match max_elm s ~cmp with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    );
  end in
  test_elm "" ~cmp:Codepoint.cmp;
  test_elm "baced" ~cmp:Codepoint.cmp;
  let cmp cp0 cp1 = begin
    match Codepoint.cmp cp0 cp1 with
    | Lt -> Cmp.Gt
    | Eq -> Cmp.Eq
    | Gt -> Cmp.Lt
  end in
  test_elm "" ~cmp;
  test_elm "baced" ~cmp

let _ = test ()
