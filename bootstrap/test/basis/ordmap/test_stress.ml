open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e ordmap = begin
    match i < n with
    | false -> ordmap
    | true -> begin
        let ordmap' = remove_hlt i (test n (succ i) e (insert_hlt ~k:i ~v:(i * 100) ordmap)) in
        assert (equal veq ordmap ordmap');
        assert (equal veq ordmap (union ~f:merge ordmap ordmap'));
        assert (equal veq ordmap (inter ~f:merge ordmap ordmap'));
        assert (equal veq e (diff ordmap ordmap'));
        validate ordmap';
        ordmap'
      end
  end in
  let e = empty (module Uns) in
  let _ = test 100 0 e e in
  printf "@]"

let _ = test ()
