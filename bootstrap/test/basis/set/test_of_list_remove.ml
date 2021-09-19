open! Basis.Rudiments
open! Basis
open SetTest
open Set
open Format

let test () =
  let geometry = get_geometry () in
  safe_set_geometry ~max_indent:100 ~margin:200;
  printf "@[";
  let test m set descr = begin
    printf "--- %s ---@\n" descr;
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Uns.pp m pp set pp (remove m set)
  end in
  let test_tuples = [
    ([0; 1], 2,          "Not member, elm empty.");
    ([1], 91,            "Not member, elm of different value.");
    ([0], 0,             "Member, length 1 -> 0.");
    ([0; 1], 1,          "Member, length 2 -> 1.");
    ([0; 1; 2], 2,       "Member, length 3 -> 2.");
    ([0; 1; 66], 66,     "Member, subnode elms 2 -> 1.");
    ([0; 1; 66; 91], 91, "Member, subnode elms 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let set = of_list (module Uns) ms in
    test m set descr
  );
  printf "@]";
  safe_set_geometry ~max_indent:geometry.max_indent ~margin:geometry.margin

let _ = test ()
