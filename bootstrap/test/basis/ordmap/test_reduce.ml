open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test ks = begin
    let ordmap = of_klist ks in
    let union = reduce ~f:Bitset.union ordmap in
    File.Fmt.stdout
    |> Fmt.fmt "reduce ~f:Bitset.union "
    |> (List.pp Uns.pp) ks
    |> Fmt.fmt " -> "
    |> (Option.fmt Bitset.pp) union
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 66L];
    [0L; 1L; 66L; 91L];
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  )

let _ = test ()
