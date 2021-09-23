open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let t_map, f_map = partition_tf map ~f:(fun (k, _) -> k % 2L = 0L) in
    let t_kvs = to_alist t_map in
    let f_kvs = to_alist f_map in
    printf "%a -> %a / %a@\n"
      (List.pp Uns.pp) ks
      (List.pp (pp_kv Uns.pp)) t_kvs
      (List.pp (pp_kv Uns.pp)) f_kvs
  end in
  iter_oc 0L 7L (fun n ->
    let ks = Array.(to_list (init n ~f:(fun i -> i))) in
    test ks
  );
  printf "@]"

let _ = test ()
