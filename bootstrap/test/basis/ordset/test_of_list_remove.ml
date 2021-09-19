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
      Uns.pp m pp ordset pp ordset'
  end in
  let test_tuples = [
    ([0; 1], 2,            "Not member.");
    ([0], 0,               "Member, length 1 -> 0.");
    ([0; 1], 1,            "Member, length 2 -> 1.");
    ([0; 1; 2], 2,         "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let ordset = of_list (module Uns) ms in
    test m ordset descr
  );
  printf "@]"

let _ = test ()
