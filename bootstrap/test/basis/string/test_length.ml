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
      pp s
      Uns.pp (B.length s)
      Uns.pp (C.length s)
      (is_empty s)
  )

let _ = test ()
