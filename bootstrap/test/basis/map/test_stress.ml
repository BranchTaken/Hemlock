open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e map = begin
    match i < n with
    | false -> map
    | true -> begin
        let map' = remove_hlt i
            (test n (succ i) e (insert_hlt ~k:i ~v:(i * 100L) map)) in
        assert (equal veq map map');
        assert (equal veq map (union ~f:merge map map'));
        assert (equal veq map (inter ~f:merge map map'));
        assert (equal veq e (diff map map'));
        validate map';
        map'
      end
  end in
  let e = empty (module UnsTestCmper) in
  let _ = test 100L 0L e e in
  ()

let _ = test ()
