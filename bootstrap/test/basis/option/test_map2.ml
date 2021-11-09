open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  let xpp_ab xppf (a, b) = fprintf xppf "(%a, %a)" Uns.xpp a String.xpp b in
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      printf "map2 (%a) (%a) ~f:(fun a b -> (a, b)) -> %a\n"
        (xpp Uns.xpp) o0
        (xpp String.xpp) o1
        (xpp xpp_ab) (map2 o0 o1 ~f:(fun a b -> (a, b)))
    )
  );
  printf "@]"

let _ = test ()
