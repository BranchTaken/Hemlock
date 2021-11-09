open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_foldi s = begin
    printf "foldi %a ->" xpp s;
    let () = foldi s ~init:() ~f:(fun i _ cp ->
      printf " %a:%s" Uns.xpp i (of_codepoint cp)) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_foldi

let _ = test ()
