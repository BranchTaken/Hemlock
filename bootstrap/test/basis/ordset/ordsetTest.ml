open! Basis.Rudiments
open! Basis
open Ordset

let pp ppf t =
  let open Format in
  fprintf ppf "@[<h>Ordset {";
  let cmper = cmper t in
  let t_sorted = Array.sort ~cmp:cmper.cmp (to_array t) in
  Array.iteri t_sorted ~f:(fun i a ->
    if i > 0L then fprintf ppf ";@ ";
    fprintf ppf "%a" cmper.pp a
  );
  fprintf ppf "}@]"
