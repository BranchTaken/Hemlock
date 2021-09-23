open! Basis.Rudiments
open! Basis
open StringTest
open String
open Format

let test () =
  let strs = [
    "";
    "<_>";
    "«»";
  ] in
  List.iter strs ~f:(fun s ->
    iter_oc 0L (clength s + 2L) (fun i ->
      printf "prefix %a %a -> %a\n" pp s Uns.pp i pp (prefix i s);
      printf "suffix %a %a -> %a\n" pp s Uns.pp i pp (suffix i s);
    )
  )

let _ = test ()
