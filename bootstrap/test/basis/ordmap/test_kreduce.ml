open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test ks = begin
    let ordmap = of_klist ks in
    let sum = kreduce ~f:( + ) ordmap in
    File.Fmt.stdout
    |> Fmt.fmt "kreduce ~f:( + ) "
    |> (List.pp Uns.pp) ks
    |> Fmt.fmt " -> "
    |> (Option.fmt Uns.pp) sum
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
