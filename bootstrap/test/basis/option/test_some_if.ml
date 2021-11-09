open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  List.iter [false; true] ~f:(fun b ->
    let a = 42L in
    printf "some_if %b %a -> %a\n"
      b
      Uns.xpp a
      (xpp Uns.xpp) (some_if b a)
  );
  printf "@]"

let _ = test ()
