open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test ks = begin
    let ordmap = of_klist ks in
    let sum = reduce ~f:( + ) ordmap in
    printf "reduce ~f:( + ) %a -> %a\n"
      (List.pp Uns.pp) ks
      (Option.pp Uns.pp) sum
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  );
  printf "@]"

let _ = test ()
