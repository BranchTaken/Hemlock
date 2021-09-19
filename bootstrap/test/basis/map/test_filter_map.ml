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
      match k % 2 = 0 with
      | true -> Some (Uns.to_string v)
      | false -> None
    ) in
    let kvs = to_alist map' in
    printf "%a -> %a@\n"
      (List.pp Uns.pp) ks
      (List.pp (pp_kv String.pp)) kvs
  end in
  for n = 0 to 6 do
    let ks = Array.(to_list (init n ~f:(fun i -> i))) in
    test ks
  done;
  printf "@]"

let _ = test ()
