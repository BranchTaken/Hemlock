open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  let test m ordset descr = begin
    printf "--- %s ---@\n" descr;
    let ordset' = remove m ordset in
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Uns.xpp m xpp ordset xpp ordset'
  end in
  let test_tuples = [
    ([0L; 1L], 2L,             "Not member.");
    ([0L], 0L,                 "Member, length 1 -> 0.");
    ([0L; 1L], 1L,             "Member, length 2 -> 1.");
    ([0L; 1L; 2L], 2L,         "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let ordset = of_list (module Uns) ms in
    test m ordset descr
  );
  printf "@]"

let _ = test ()
