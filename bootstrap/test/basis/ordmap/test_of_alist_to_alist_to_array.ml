open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test kvs = begin
    let ordmap = of_alist (module Uns) kvs in
    printf "of_alist %a; to_alist -> %a; to_array -> %a\n"
      (List.pp (pp_kv String.pp)) kvs
      (List.pp (pp_kv String.pp)) (to_alist ordmap)
      (Array.pp (pp_kv String.pp)) (to_array ordmap)
  end in
  let test_alists = [
    [];
    [(0, "0")];
    [(0, "0"); (1, "1")];
    [(0, "0"); (1, "1"); (2, "2")];
    [(0, "0"); (1, "1"); (66, "66")];
    [(0, "0"); (1, "1"); (66, "66"); (91, "91")];
  ] in
  List.iter test_alists ~f:(fun kvs ->
    test kvs
  );
  printf "@]"

let _ = test ()
