open! Basis.Rudiments
open! Basis
open Set

let xpp xppf t =
  let open Format in
  fprintf xppf "@[<h>Set {";
  let cmper = cmper t in
  let t_sorted = Array.sort ~cmp:cmper.cmp (to_array t) in
  Array.iteri t_sorted ~f:(fun i a ->
    if i > 0L then fprintf xppf ";@ ";
    fprintf xppf "%a" cmper.xpp a
  );
  fprintf xppf "}@]"
