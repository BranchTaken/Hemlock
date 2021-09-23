open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_map uarr = begin
    printf "%a -> map " (pp Uns.pp) uarr;
    let sarr = map uarr ~f:(fun elm -> asprintf "%a" Uns.pp elm) in
    printf "%a" (pp pp_str) sarr;
    printf " -> mapi ";
    let sarr = mapi uarr ~f:(fun i elm ->
      asprintf "[%a]=%a" Uns.pp i Uns.pp elm
    ) in
    printf "%a\n" (pp pp_str) sarr
  end in
  printf "@[<h>";
  test_map [||];
  test_map [|0L|];
  test_map [|1L; 0L|];
  test_map [|2L; 1L; 0L|];
  printf "@]"

let _ = test ()
