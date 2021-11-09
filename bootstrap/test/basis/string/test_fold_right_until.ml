open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_fold_right_until s = begin
    printf "fold_right_until %a ->" xpp s;
    let () = fold_right_until s ~init:() ~f:(fun cp _ ->
      let until = Codepoint.(cp = (of_char 'c')) in
      printf " %s" (of_codepoint cp);
      (), until
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_fold_right_until

let _ = test ()
