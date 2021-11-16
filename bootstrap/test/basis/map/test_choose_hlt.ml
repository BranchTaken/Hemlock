open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i map = begin
    match i < n with
    | false -> map
    | true -> begin
        validate map;
        let map' = test n (succ i) (insert_hlt ~k:i ~v:(i * 100L) map) in
        let k, v = choose_hlt map' in
        assert (k * 100L = v);
        let map'' = remove_hlt k map' in
        validate map'';
        assert ((length map') = (length map'') + 1L);
        map''
      end
  end in
  let e = empty (module UnsTestCmper) in
  let _ = test 100L 0L e in
  ()

let _ = test ()
