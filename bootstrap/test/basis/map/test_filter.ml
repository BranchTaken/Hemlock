open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let map' = filter map ~f:(fun (k, _) -> k % 2L = 0L) in
    let kvs = to_alist map' in
    printf "%a -> %a@\n"
      (List.xpp Uns.xpp) ks
      (List.xpp (xpp_kv Uns.xpp)) kvs
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  );
  printf "@]"

let _ = test ()
