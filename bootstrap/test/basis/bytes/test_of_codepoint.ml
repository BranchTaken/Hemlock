open! Basis.Rudiments
open! Basis
open Bytes
open Format

let test () =
  let strs = [
    "<";
    "«";
    "‡";
    "𐆗";
  ] in
  let cps = List.fold_right strs ~init:[] ~f:(fun s cps ->
    String.C.Cursor.(rget (hd s)) :: cps
  ) in
  printf "@[<h>";
  List.iter cps ~f:(fun cp ->
    let bytes = of_codepoint cp in
    printf "'%s' -> %a -> %a\n"
      (String.of_codepoint cp)
      xpp bytes
      String.xpp (to_string_hlt bytes)
  );
  printf "@]"

let _ = test ()
