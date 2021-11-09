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
      (List.xpp (xpp_kv String.xpp)) kvs
      (List.xpp (xpp_kv String.xpp)) (to_alist ordmap)
      (Array.xpp (xpp_kv String.xpp)) (to_array ordmap)
  end in
  let test_alists = [
    [];
    [(0L, "0")];
    [(0L, "0"); (1L, "1")];
    [(0L, "0"); (1L, "1"); (2L, "2")];
    [(0L, "0"); (1L, "1"); (66L, "66")];
    [(0L, "0"); (1L, "1"); (66L, "66"); (91L, "91")];
  ] in
  List.iter test_alists ~f:(fun kvs ->
    test kvs
  );
  printf "@]"

let _ = test ()
