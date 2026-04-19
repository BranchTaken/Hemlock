open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let kshift = 128L

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e ordmap = begin
    assert (n < kshift);
    match i < n with
    | false -> ordmap
    | true -> begin
        let ordmap' = remove_hlt i
            (test n (succ i) e (insert_hlt ~k:i ~v:(Bitset.singleton (i + kshift)) ordmap)) in
        assert (equal ~vequal ordmap ordmap');
        assert (equal ~vequal ordmap (union ~vunion ordmap ordmap'));
        assert (equal ~vequal ordmap (inter ~vinter ordmap ordmap'));
        assert (equal ~vequal e (diff ~vdiff ordmap ordmap'));
        validate ordmap';
        ordmap'
      end
  end in
  let e = empty (module Uns) in
  let _ = test 100L 0L e e in
  ()

let _ = test ()
