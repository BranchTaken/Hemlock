open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_fold_right s = begin
    printf "fold_right %a ->" pp s;
    let () = fold_right s ~init:() ~f:(fun cp _ ->
      printf " %s" (of_codepoint cp)
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_fold_right

let _ = test ()
