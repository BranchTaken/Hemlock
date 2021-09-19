open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  printf "@[<h>";
  let f i accum elm = (elm :: accum), (elm + i * 10) in
  iter lists ~f:(fun l ->
    let accum, b_list = foldi_map l ~init:[] ~f in
    printf "    fold_mapi %a -> accum=%a, b_list=%a\n"
      (pp Uns.pp) l
      (pp Uns.pp) accum
      (pp Uns.pp) b_list
    ;

    let accum, b_list = rev_foldi_map l ~init:[] ~f in
    printf "rev_fold_mapi %a -> accum=%a, b_list=%a\n"
      (pp Uns.pp) l
      (pp Uns.pp) accum
      (pp Uns.pp) b_list
  );
  printf "@]"

let _ = test ()
