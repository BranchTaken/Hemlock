open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  let test k ordmap descr = begin
    validate ordmap;
    printf "--- %s ---@\n" descr;
    let ordmap' = remove_hlt k ordmap in
    validate ordmap';
    printf "@[<v>remove_hlt %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Uns.pp k (pp String.pp) ordmap (pp String.pp) ordmap'
  end in
  let test_tuples = [
    ([(0, "0")], 0,                     "Member, length 1 -> 0.");
    ([(0, "0"); (1, "1")], 1,           "Member, length 2 -> 1.");
    ([(0, "0"); (1, "1"); (2, "2")], 2, "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (kvs, k, descr) ->
    let ordmap = of_alist (module Uns) kvs in
    test k ordmap descr
  );
  printf "@]"

let _ = test ()
