open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_lists = [
    [];

    [[]];
    [[0; 1]];

    [[]; []];
    [[0; 1]; [2; 3]];

    [[]; []; []];
    [[0; 1]; [2; 3]; [4; 5]];
  ] in
  printf "@[<h>";
  iter list_lists ~f:(fun lists ->
    printf "join";
    iter lists ~f:(fun l -> printf " %a" (pp Uns.pp) l);
    printf " -> %a\n" (pp Uns.pp) (join lists);

    printf "join ~sep:[6; 7]";
    iter lists ~f:(fun l -> printf " %a" (pp Uns.pp) l);
    printf " -> %a\n" (pp Uns.pp) (join ~sep:[6; 7] lists);

    (* Brittle test; change in conjunction with implementation. *)
    printf "join_unordered ~sep:[6; 7]";
    iter lists ~f:(fun l -> printf " %a" (pp Uns.pp) l);
    printf " -> %a\n" (pp Uns.pp) (join_unordered ~sep:[6; 7] lists);
  );
  printf "@]"

let _ = test ()
