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
        let v = k * 100L in
        let map' = update ~k ~v map in
        assert (not (mem k map'));
        validate map';
        (* upsert *)
        let map'' = upsert ~k ~v map' in
        assert (mem k map'');
        assert ((get_hlt k map'') = v);
        validate map'';
        (* update_hlt *)
        let v' = k * 10000L in
        let map''' = update_hlt ~k ~v:v' map'' in
        assert (mem k map''');
        assert ((get_hlt k map''') = v');
        assert (not (subset veq map'' map'''));
        assert (not (subset veq map''' map''));
        validate map''';
        (* update *)
        let v'' = k * 1000000L in
        let map'''' = update ~k ~v:v'' map''' in
        assert (mem k map'''');
        assert ((get_hlt k map'''') = v'');
        validate map'''';
        test ks' map''''
      end
  end in
  let ks = [1L; 3L; 2L; 42L; 44L; 45L; 56L; 60L; 66L; 75L; 81L; 91L; 420L; 421L; 4200L] in
  test ks (empty (module UnsTestCmper))

let _ = test ()
