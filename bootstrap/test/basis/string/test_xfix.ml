open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let strs = [
    "";
    "<_>";
    "«»";
  ] in
  List.iter strs ~f:(fun s ->
    for i = 0 to (clength s) + 1 do
      printf "prefix %a %a -> %a\n" pp s Uns.pp i pp (prefix i s);
      printf "suffix %a %a -> %a\n" pp s Uns.pp i pp (suffix i s);
    done
  )

let _ = test ()
