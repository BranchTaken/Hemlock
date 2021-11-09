open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o ->
    printf "is_some %a -> %b\n" (xpp Uns.xpp) o (is_some o);
    printf "is_none %a -> %b\n" (xpp Uns.xpp) o (is_none o);
  );
  printf "@]"

let _ = test ()
