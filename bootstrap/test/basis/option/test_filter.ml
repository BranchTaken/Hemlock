open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  List.iter [false; true] ~f:(fun b ->
    List.iter [Some 42L; None] ~f:(fun o ->
      printf "filter %a ~f:(fun _ -> %b) -> %a\n"
        (pp Uns.pp) o
        b
        (pp Uns.pp) (filter o ~f:(fun _ -> b))
    )
  );
  printf "@]"

let _ = test ()
