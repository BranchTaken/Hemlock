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
        let v = k * 100L in
        let map' = amend k ~f:(function
          | None -> Some v
          | Some _ -> not_reached ()
        ) map in
        assert (mem k map');
        assert ((get_hlt k map') = v);
        validate map';
        let v' = k * 10000L in
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
  let ks = [1L; 3L; 2L; 42L; 44L; 45L; 56L; 60L; 66L; 75L; 81L; 91L; 420L; 421L; 4200L] in
  test ks (empty (module UnsTestCmper))

let _ = test ()
