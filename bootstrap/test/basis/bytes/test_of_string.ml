open! Basis.Rudiments
open! Basis
open Bytes
open Format

let test () =
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  printf "@[<h>";
  List.iter strs ~f:(fun s ->
    let bytes = of_string_slice (String.C.Slice.of_string s) in
    printf "%a -> %a -> %a\n"
      String.xpp s
      xpp bytes
      String.xpp (to_string_hlt bytes)
  );
  printf "@]"

let _ = test ()
