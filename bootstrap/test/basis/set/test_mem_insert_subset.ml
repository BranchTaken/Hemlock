open! Basis.Rudiments
open! Basis
open SetTest
open Set
open Format

let test () =
  printf "@[";
  let rec test ms set = begin
    match ms with
    | [] -> printf "%a@\n" pp set
    | m :: ms' -> begin
        assert (not (mem m set));
        let set' = insert m set in
        assert (mem m set');
        assert (subset set' set);
        assert (not (subset set set'));
        test ms' set'
      end
  end in
  let ms = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ms (empty (module Uns));
  printf "@]"

let _ = test ()
