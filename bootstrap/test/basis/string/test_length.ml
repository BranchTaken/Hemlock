open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let strs = [
    "";
    "<_>";
    "«»";
    "‡";
    "𐆗";
  ] in
  List.iter strs ~f:(fun s ->
    printf "s=%a, blength=%a, clength=%a, is_empty=%B\n"
      xpp s
      Uns.xpp (B.length s)
      Uns.xpp (C.length s)
      (is_empty s)
  )

let _ = test ()
