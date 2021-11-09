open! Basis.Rudiments
open! Basis
open MapTest
open Map
open Format

let test () =
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let sum = kreduce ~f:( + ) map in
    printf "kreduce ~f:( + ) %a -> %a\n"
      (List.xpp Uns.xpp) ks
      (Option.xpp Uns.xpp) sum
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
  );
  printf "@]"

let _ = test ()
