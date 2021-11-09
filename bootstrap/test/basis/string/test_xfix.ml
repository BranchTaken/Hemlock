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
    Range.iter (0L =:< (C.length s + 2L)) ~f:(fun i ->
      printf "prefix %a %a -> %a\n" xpp s Uns.xpp i xpp (prefix i s);
      printf "suffix %a %a -> %a\n" xpp s Uns.xpp i xpp (suffix i s);
    )
  )

let _ = test ()
