open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  printf "@[<h>";
  let replacement = 77L in
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some 43L; None] ~f:(fun o1 ->
      printf "merge (%a) (%a) ~f:(fun _ _ -> %a) -> %a\n"
        (pp Uns.pp) o0
        (pp Uns.pp) o1
        Uns.pp replacement
        (pp Uns.pp) (merge o0 o1 ~f:(fun _ _ -> replacement))
    )
  );
  printf "@]"

let _ = test ()
