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
        let map' = amend k ~f:(function
          | None -> Some v
          | Some _ -> not_reached ()
        ) map in
        assert (mem k map');
        assert ((get_hlt k map') = v);
        validate map';
        let v' = k * 10000 in
        let map'' = amend k ~f:(function
          | Some vx -> begin
              assert (vx = v);
              Some v'
            end
          | None -> not_reached ()
        ) map' in
        assert (mem k map'');
        assert ((get_hlt k map'') = v');
        validate map'';
        test ks' map''
      end
  end in
  let ks = [1; 3; 2; 42; 44; 45; 56; 60; 66; 75; 81; 91; 420; 421; 4200] in
  test ks (empty (module UnsTestCmper))

let _ = test ()
