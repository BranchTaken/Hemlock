open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  let default = 13L in
  let replacement = 43L in
  List.iter [Some 42L; None] ~f:(fun o ->
    printf "value_map %a ~default:%a ~f:(fun _ -> %a) -> %a\n"
      (pp Uns.pp) o
      Uns.pp default
      Uns.pp replacement
      Uns.pp (value_map o ~default ~f:(fun _ -> replacement))
  );
  printf "@]"

let _ = test ()
