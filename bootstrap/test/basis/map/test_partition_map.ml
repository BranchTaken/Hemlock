open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let a_map, b_map = partition_map map ~f:(fun (k, v) ->
      match k % 2L = 0L with
      | true -> First (Uns.to_string v)
      | false -> Second (Uns.bits_to_sint v)
    ) in
    let a_kvs = to_alist a_map in
    let b_kvs = to_alist b_map in
    printf "%a -> %a / %a@\n"
      (List.xpp Uns.xpp) ks
      (List.xpp (xpp_kv String.xpp)) a_kvs
      (List.xpp (xpp_kv Sint.xpp)) b_kvs
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  );
  printf "@]"

let _ = test ()
