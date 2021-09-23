open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  let pp_ab ppf (a, b) = fprintf ppf "(%a, %a)" Uns.pp a String.pp b in
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      printf "both (%a) (%a) -> %a\n"
        (pp Uns.pp) o0
        (pp String.pp) o1
        (pp pp_ab) (both o0 o1)
    )
  );
  printf "@]"

let _ = test ()
