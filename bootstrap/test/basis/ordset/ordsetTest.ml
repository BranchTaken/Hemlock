open! Basis.Rudiments
open! Basis
open Ordset

let iter_oc base past f =
  let rec fn i past f = begin
    match Uns.(i < past) with
    | false -> ()
    | true -> begin
        f i;
        fn (Uns.succ i) past f
      end
  end in
  fn base past f

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
