open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 2L; 3L];
  ] in
  printf "@[<h>";
  let f i accum elm = (elm :: accum), (elm + i * 10L) in
  iter lists ~f:(fun l ->
    let accum, b_list = foldi_map l ~init:[] ~f in
    printf "    fold_mapi %a -> accum=%a, b_list=%a\n"
      (xpp Uns.xpp) l
      (xpp Uns.xpp) accum
      (xpp Uns.xpp) b_list
    ;

    let accum, b_list = rev_foldi_map l ~init:[] ~f in
    printf "rev_fold_mapi %a -> accum=%a, b_list=%a\n"
      (xpp Uns.xpp) l
      (xpp Uns.xpp) accum
      (xpp Uns.xpp) b_list
  );
  printf "@]"

let _ = test ()
