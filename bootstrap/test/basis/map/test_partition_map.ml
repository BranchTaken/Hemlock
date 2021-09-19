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
      match k % 2 = 0 with
      | true -> First (Uns.to_string v)
      | false -> Second (Uns.to_sint v)
    ) in
    let a_kvs = to_alist a_map in
    let b_kvs = to_alist b_map in
    printf "%a -> %a / %a@\n"
      (List.pp Uns.pp) ks
      (List.pp (pp_kv String.pp)) a_kvs
      (List.pp (pp_kv Sint.pp)) b_kvs
  end in
  for n = 0 to 6 do
    let ks = Array.(to_list (init n ~f:(fun i -> i))) in
    test ks
  done;
  printf "@]"

let _ = test ()
