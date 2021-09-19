open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  let default = 13 in
  let replacement = 43 in
  List.iter [Some 42; None] ~f:(fun o ->
    printf "value_map %a ~default:%a ~f:(fun _ -> %a) -> %a\n"
      (pp Uns.pp) o
      Uns.pp default
      Uns.pp replacement
      Uns.pp (value_map o ~default ~f:(fun _ -> replacement))
  );
  printf "@]"

let _ = test ()
