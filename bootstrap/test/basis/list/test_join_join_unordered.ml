open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_lists = [
    [];

    [[]];
    [[0L; 1L]];

    [[]; []];
    [[0L; 1L]; [2L; 3L]];

    [[]; []; []];
    [[0L; 1L]; [2L; 3L]; [4L; 5L]];
  ] in
  printf "@[<h>";
  iter list_lists ~f:(fun lists ->
    printf "join";
    iter lists ~f:(fun l -> printf " %a" (xpp Uns.xpp) l);
    printf " -> %a\n" (xpp Uns.xpp) (join lists);

    printf "join ~sep:[6; 7]";
    iter lists ~f:(fun l -> printf " %a" (xpp Uns.xpp) l);
    printf " -> %a\n" (xpp Uns.xpp) (join ~sep:[6L; 7L] lists);

    (* Brittle test; change in conjunction with implementation. *)
    printf "join_unordered ~sep:[6; 7]";
    iter lists ~f:(fun l -> printf " %a" (xpp Uns.xpp) l);
    printf " -> %a\n" (xpp Uns.xpp) (join_unordered ~sep:[6L; 7L] lists);
  );
  printf "@]"

let _ = test ()
