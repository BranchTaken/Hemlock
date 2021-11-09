open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_fold s = begin
    printf "fold %a ->" xpp s;
    let () = fold s ~init:() ~f:(fun _ cp -> printf " %s" (of_codepoint cp)) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_fold

let _ = test ()
