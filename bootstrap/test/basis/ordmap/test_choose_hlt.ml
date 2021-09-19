open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i ordmap = begin
    match i < n with
    | false -> ordmap
    | true -> begin
        validate ordmap;
        let ordmap' = test n (succ i) (insert_hlt ~k:i ~v:(i * 100) ordmap) in
        let k, v = choose_hlt ordmap' in
        assert (k * 100 = v);
        let ordmap'' = remove_hlt k ordmap' in
        validate ordmap'';
        assert ((length ordmap') = (length ordmap'') + 1);
        ordmap''
      end
  end in
  let e = empty (module Uns) in
  let _ = test 100 0 e in
  printf "@]"

let _ = test ()
