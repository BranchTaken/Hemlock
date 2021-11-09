open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o ->
    printf "value %a -> %a\n"
      (xpp Uns.xpp) o Uns.xpp (value ~default:13L o)
  );
  printf "@]"

let _ = test ()
