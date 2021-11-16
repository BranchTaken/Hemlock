open! Basis.Rudiments
open! Basis
open SetTest
open Set

let test () =
  let rec test ms set = begin
    match ms with
    | [] -> File.Fmt.stdout |> fmt set |> Fmt.fmt "\n" |> ignore
    | m :: ms' -> begin
        assert (not (mem m set));
        let set' = insert m set in
        assert (mem m set');
        assert (subset set' set);
        assert (not (subset set set'));
        test ms' set'
      end
  end in
  let ms = [1L; 3L; 2L; 44L; 45L; 56L; 60L; 66L; 75L; 81L; 91L] in
  test ms (empty (module Uns))

let _ = test ()
