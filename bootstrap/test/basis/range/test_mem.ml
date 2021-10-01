open! Basis.Rudiments
open! Basis
open Format

let print_mem_h i range =
  let base = Range.base range in
  let past = Range.past range in
  printf "mem %Lu (%Lu .. %Lu) -> %b\n" i base past (Range.mem i range)

let print_mem_f i range =
  let base = RangeF.Uns.base range in
  let last = RangeF.Uns.last range in
  printf "mem %Lu (%Lu ..= %Lu) -> %b\n" i base last (RangeF.Uns.(mem i range))

let test () =
  printf "@[<h>";
  Range.iter (0L =:< 4L) ~f:(fun i -> print_mem_h i (1L =:< 3L));
  printf "\n";
  Range.iter (0L =:< 4L) ~f:(fun i -> print_mem_h i (3L =:< 1L));
  printf "\n";
  Range.iter (0L =:< 5L) ~f:(fun i -> print_mem_f i RangeF.Uns.(1L =:= 3L));
  printf "\n";
  Range.iter (0L =:< 3L) ~f:(fun i -> print_mem_f i RangeF.Uns.(1L =:= 1L));
  printf "\n";
  Range.iter (0L =:< 5L) ~f:(fun i -> print_mem_f i RangeF.Uns.(3L =:= 1L));

  printf "@]"

let _ = test ()
