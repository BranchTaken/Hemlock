open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  let pp_ab ppf (a, b) = fprintf ppf "(%a, %a)" Uns.pp a String.pp b in
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      printf "map2 (%a) (%a) ~f:(fun a b -> (a, b)) -> %a\n"
        (pp Uns.pp) o0
        (pp String.pp) o1
        (pp pp_ab) (map2 o0 o1 ~f:(fun a b -> (a, b)))
    )
  );
  printf "@]"

let _ = test ()
