open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_iteri s = begin
    printf "iteri %a ->" xpp s;
    let () = iteri s ~f:(fun i cp ->
      printf " %a:%s" Uns.xpp i (of_codepoint cp)
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_iteri

let _ = test ()
