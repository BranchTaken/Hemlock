open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let rec test ks map = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k map));
        assert (Option.is_none (get k map));
        let v = k * 100 in
        let map' = insert ~k ~v map in
        validate map';
        assert (mem k map');
        assert ((get_hlt k map') = v);
        assert (subset veq map' map);
        assert (not (subset veq map map'));
        test ks' map'
      end
  end in
  let ks = [1; 3; 2; 42; 44; 45; 56; 60; 66; 75; 81; 91; 420; 421; 4200] in
  test ks (empty (module UnsTestCmper))

let _ = test ()
