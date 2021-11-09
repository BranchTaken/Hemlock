open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_iter s = begin
    printf "iter %a ->" xpp s;
    let () = iter s ~f:(fun cp -> printf " %s" (of_codepoint cp)) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_iter

let _ = test ()
