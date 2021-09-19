open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let rec test ks ordmap = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k ordmap));
        assert (Option.is_none (get k ordmap));
        let v = k * 100 in
        let ordmap' = amend k ~f:(function
          | None -> Some v
          | Some _ -> not_reached ()
        ) ordmap in
        assert (mem k ordmap');
        assert ((get_hlt k ordmap') = v);
        validate ordmap';
        let v' = k * 10000 in
        let ordmap'' = amend k ~f:(function
          | Some vx -> begin
              assert (vx = v);
              Some v'
            end
          | None -> not_reached ()
        ) ordmap' in
        assert (mem k ordmap'');
        assert ((get_hlt k ordmap'') = v');
        validate ordmap'';
        test ks' ordmap''
      end
  end in
  let ks = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ks (empty (module Uns))

let _ = test ()
