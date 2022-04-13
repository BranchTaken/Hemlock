open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let rec test ms ordset = begin
    match ms with
    | [] -> File.Fmt.stdout |> fmt ordset |> Fmt.fmt "\n" |> ignore
    | m :: ms' -> begin
        assert (not (mem m ordset));
        let ordset' = insert m ordset in
        assert (mem m ordset');
        assert (subset ordset' ordset);
        assert (not (subset ordset ordset'));
        test ms' ordset'
      end
  end in
  let ms = [1L; 3L; 2L; 44L; 45L; 56L; 60L; 66L; 75L; 81L; 91L] in
  test ms (empty (module Uns))

let _ = test ()
