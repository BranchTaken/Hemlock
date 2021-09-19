open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  List.iter [false; true] ~f:(fun b ->
    let a = 42 in
    printf "some_if %b %a -> %a\n"
      b
      Uns.pp a
      (pp Uns.pp) (some_if b a)
  );
  printf "@]"

let _ = test ()
