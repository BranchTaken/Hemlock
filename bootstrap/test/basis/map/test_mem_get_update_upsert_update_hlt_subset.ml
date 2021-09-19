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
        (* update (silently fail) *)
        let v = k * 100 in
        let map' = update ~k ~v map in
        assert (not (mem k map'));
        validate map';
        (* upsert *)
        let map'' = upsert ~k ~v map' in
        assert (mem k map'');
        assert ((get_hlt k map'') = v);
        validate map'';
        (* update_hlt *)
        let v' = k * 10000 in
        let map''' = update_hlt ~k ~v:v' map'' in
        assert (mem k map''');
        assert ((get_hlt k map''') = v');
        assert (not (subset veq map'' map'''));
        assert (not (subset veq map''' map''));
        validate map''';
        (* update *)
        let v'' = k * 1000000 in
        let map'''' = update ~k ~v:v'' map''' in
        assert (mem k map'''');
        assert ((get_hlt k map'''') = v'');
        validate map'''';
        test ks' map''''
      end
  end in
  let ks = [1; 3; 2; 42; 44; 45; 56; 60; 66; 75; 81; 91; 420; 421; 4200] in
  test ks (empty (module UnsTestCmper))

let _ = test ()
