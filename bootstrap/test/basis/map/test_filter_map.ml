open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let map' = filter_map map ~f:(fun (k, v) ->
      match k % 2L = 0L with
      | true -> Some (Uns.to_string v)
      | false -> None
    ) in
    let kvs = to_alist map' in
    printf "%a -> %a@\n"
      (List.pp Uns.pp) ks
      (List.pp (pp_kv String.pp)) kvs
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  );
  printf "@]"

let _ = test ()
