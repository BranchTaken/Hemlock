open! Basis.Rudiments
open! Basis

let print s =
  File.Fmt.stdout |> Fmt.fmt s |> ignore

let print_mem_h i range =
  File.Fmt.stdout
  |> Fmt.fmt "mem "
  |> Uns.fmt i
  |> Fmt.fmt " ("
  |> Uns.fmt (Range.base range)
  |> Fmt.fmt " .. "
  |> Uns.fmt (Range.past range)
  |> Fmt.fmt ") -> "
  |> Bool.fmt (Range.mem i range)
  |> Fmt.fmt "\n"
  |> ignore

let print_mem_f i range =
  File.Fmt.stdout
  |> Fmt.fmt "mem "
  |> Uns.fmt i
  |> Fmt.fmt " ("
  |> Uns.fmt (RangeF.Uns.base range)
  |> Fmt.fmt " ..= "
  |> Uns.fmt (RangeF.Uns.last range)
  |> Fmt.fmt ") -> "
  |> Bool.fmt (RangeF.Uns.mem i range)
  |> Fmt.fmt "\n"
  |> ignore

let test () =
  Range.iter (0L =:< 4L) ~f:(fun i -> print_mem_h i (1L =:< 3L));
  print "\n";
  Range.iter (0L =:< 4L) ~f:(fun i -> print_mem_h i (3L =:< 1L));
  print "\n";
  Range.iter (0L =:< 5L) ~f:(fun i -> print_mem_f i RangeF.Uns.(1L =:= 3L));
  print "\n";
  Range.iter (0L =:< 3L) ~f:(fun i -> print_mem_f i RangeF.Uns.(1L =:= 1L));
  print "\n";
  Range.iter (0L =:< 5L) ~f:(fun i -> print_mem_f i RangeF.Uns.(3L =:= 1L))

let _ = test ()
