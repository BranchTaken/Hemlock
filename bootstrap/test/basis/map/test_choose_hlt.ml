open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i map = begin
    match i < n with
    | false -> map
    | true -> begin
        validate map;
        let map' = test n (succ i) (insert_hlt ~k:i ~v:(i * 100) map) in
        let k, v = choose_hlt map' in
        assert (k * 100 = v);
        let map'' = remove_hlt k map' in
        validate map'';
        assert ((length map') = (length map'') + 1);
        map''
      end
  end in
  let e = empty (module UnsTestCmper) in
  let _ = test 100 0 e in
  printf "@]"

let _ = test ()
