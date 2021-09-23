open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o ->
    printf "value %a -> %a\n"
      (pp Uns.pp) o Uns.pp (value ~default:13L o)
  );
  printf "@]"

let _ = test ()
