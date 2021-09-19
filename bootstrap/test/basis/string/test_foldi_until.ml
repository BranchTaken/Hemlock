open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_foldi_until s = begin
    printf "foldi_until %a ->" pp s;
    let () = foldi_until s ~init:() ~f:(fun i _ cp ->
      let until = Codepoint.(cp = (of_char 'c')) in
      printf " %a:%s" Uns.pp i (of_codepoint cp);
      (), until
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_foldi_until

let _ = test ()
