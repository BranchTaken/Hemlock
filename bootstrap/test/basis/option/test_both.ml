open! Basis.Rudiments
open! Basis
open Option
open Format

let test () =
  let xpp_ab xppf (a, b) = fprintf xppf "(%a, %a)" Uns.xpp a String.xpp b in
  printf "@[<h>";
  List.iter [Some 42L; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      printf "both (%a) (%a) -> %a\n"
        (xpp Uns.xpp) o0
        (xpp String.xpp) o1
        (xpp xpp_ab) (both o0 o1)
    )
  );
  printf "@]"

let _ = test ()
