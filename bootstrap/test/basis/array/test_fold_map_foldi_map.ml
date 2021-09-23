open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_fold_map uarr = begin
    let accum, sarr = fold_map uarr ~init:0L ~f:(fun accum elm ->
      (accum + elm), (asprintf "%a" Uns.pp elm)
    ) in
    let accum2, sarr2 = foldi_map uarr ~init:0L ~f:(fun i accum elm ->
      (accum + i + elm),
      (asprintf "[%a]=%a" Uns.pp i Uns.pp elm)
    ) in
    printf "%a -> fold_map %a %a -> foldi_map %a %a\n"
      (pp Uns.pp) uarr
      Uns.pp accum
      (pp pp_str) sarr
      Uns.pp accum2
      (pp pp_str) sarr2
  end in
  printf "@[<h>";
  test_fold_map [||];
  test_fold_map [|0L|];
  test_fold_map [|1L; 0L|];
  test_fold_map [|2L; 1L; 0L|];
  printf "@]"

let _ = test ()
