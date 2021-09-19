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
        (* update (silently fail) *)
        let v = k * 100 in
        let ordmap' = update ~k ~v ordmap in
        assert (not (mem k ordmap'));
        validate ordmap';
        (* upsert *)
        let ordmap'' = upsert ~k ~v ordmap' in
        assert (mem k ordmap'');
        assert ((get_hlt k ordmap'') = v);
        validate ordmap'';
        (* update_hlt *)
        let v' = k * 10000 in
        let ordmap''' = update_hlt ~k ~v:v' ordmap'' in
        assert (mem k ordmap''');
        assert ((get_hlt k ordmap''') = v');
        assert (not (subset veq ordmap'' ordmap'''));
        assert (not (subset veq ordmap''' ordmap''));
        validate ordmap''';
        (* update *)
        let v'' = k * 1000000 in
        let ordmap'''' = update ~k ~v:v'' ordmap''' in
        assert (mem k ordmap'''');
        assert ((get_hlt k ordmap'''') = v'');
        validate ordmap'''';
        test ks' ordmap''''
      end
  end in
  let ks = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ks (empty (module Uns))

let _ = test ()
