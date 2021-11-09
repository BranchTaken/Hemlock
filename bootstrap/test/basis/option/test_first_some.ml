open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some 13L; None] ~f:(fun o1 ->
      printf "first_some (%a) (%a) -> %a\n"
        (xpp Uns.xpp) o0
        (xpp Uns.xpp) o1
        (xpp Uns.xpp) (first_some o0 o1)
    )
  );
  printf "@]"

let _ = test ()
