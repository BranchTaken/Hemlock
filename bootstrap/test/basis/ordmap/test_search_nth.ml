open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  let test_search ordmap (key_max:uns) = begin
    printf "%a@\n" (xpp Uns.xpp) ordmap;
    Range.iter (0L =:< (succ key_max)) ~f:(fun probe ->
      let open Cmp in
      printf "  %a -> %s, %s, %s@\n" Uns.xpp probe
        (match psearch probe ordmap with
          | None -> "<"
          | Some (Lt, i) -> asprintf "<[%a]=%a" Uns.xpp i (xpp_kv Uns.xpp) (nth i ordmap)
          | Some (Eq, i) -> asprintf "=[%a]=%a" Uns.xpp i (xpp_kv Uns.xpp) (nth i ordmap)
          | Some (Gt, i) -> asprintf ">[%a]=%a" Uns.xpp i (xpp_kv Uns.xpp) (nth i ordmap)
        )
        (match search probe ordmap with
          | None -> "<>"
          | Some i -> asprintf "=%a" (xpp_kv Uns.xpp) (nth i ordmap)
        )
        (match nsearch probe ordmap with
          | Some (Lt, i) -> asprintf "<[%a]=%a" Uns.xpp i (xpp_kv Uns.xpp) (nth i ordmap)
          | Some (Eq, i) -> asprintf "=[%a]=%a" Uns.xpp i (xpp_kv Uns.xpp) (nth i ordmap)
          | Some (Gt, i) -> asprintf ">[%a]=%a" Uns.xpp i (xpp_kv Uns.xpp) (nth i ordmap)
          | None -> ">"
        );
    );
  end in
  printf "@[";
  Range.iter (0L =:< 4L) ~f:(fun len ->
    let ordmap = of_array (module Uns)
      (Array.init (0L =:< len) ~f:(fun i -> let k = (i * 2L + 1L) in k, k * 10L)) in
    let key_max = len * 2L in
    test_search ordmap key_max
  );
  printf "@]"

let _ = test ()
