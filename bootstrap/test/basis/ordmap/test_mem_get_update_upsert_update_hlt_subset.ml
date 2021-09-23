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
        let v = k * 100L in
        let ordmap' = update ~k ~v ordmap in
        assert (not (mem k ordmap'));
        validate ordmap';
        (* upsert *)
        let ordmap'' = upsert ~k ~v ordmap' in
        assert (mem k ordmap'');
        assert ((get_hlt k ordmap'') = v);
        validate ordmap'';
        (* update_hlt *)
        let v' = k * 10000L in
        let ordmap''' = update_hlt ~k ~v:v' ordmap'' in
        assert (mem k ordmap''');
        assert ((get_hlt k ordmap''') = v');
        assert (not (subset veq ordmap'' ordmap'''));
        assert (not (subset veq ordmap''' ordmap''));
        validate ordmap''';
        (* update *)
        let v'' = k * 1000000L in
        let ordmap'''' = update ~k ~v:v'' ordmap''' in
        assert (mem k ordmap'''');
        assert ((get_hlt k ordmap'''') = v'');
        validate ordmap'''';
        test ks' ordmap''''
      end
  end in
  let ks = [1L; 3L; 2L; 44L; 45L; 56L; 60L; 66L; 75L; 81L; 91L] in
  test ks (empty (module Uns))

let _ = test ()
