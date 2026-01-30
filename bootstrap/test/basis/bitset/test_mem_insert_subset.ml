open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let rec test ms bitset = begin
    match ms with
    | [] -> File.Fmt.stdout |> fmt bitset |> Fmt.fmt "\n" |> ignore
    | m :: ms' -> begin
        assert (not (mem m bitset));
        let bitset' = insert m bitset in
        assert (mem m bitset');
        assert (subset bitset' bitset);
        assert (not (subset bitset bitset'));
        test ms' bitset'
      end
  end in
  let ms = [1L; 3L; 2L; 44L; 45L; 56L; 60L; 66L; 75L; 81L; 91L] in
  test ms empty

let _ = test ()
