open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_iter_right s = begin
    printf "iter_right %a ->" xpp s;
    let () = iter_right s ~f:(fun cp -> printf " %s" (of_codepoint cp)) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_iter_right

let _ = test ()
