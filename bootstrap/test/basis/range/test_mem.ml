open! Basis.Rudiments
open! Basis

let print s =
  File.Fmt.stdout |> Fmt.fmt s |> ignore

let print_mem i range =
  File.Fmt.stdout
  |> Fmt.fmt "mem "
  |> Uns.fmt i
  |> Fmt.fmt " "
  |> Range.Uns.pp range
  |> Fmt.fmt " -> "
  |> Bool.fmt (Range.Uns.mem i range)
  |> Fmt.fmt "\n"
  |> ignore

let test () =
  Range.Uns.iter (0L =:< 4L) ~f:(fun i -> print_mem i (1L =:< 3L));
  print "\n";
  Range.Uns.iter (0L =:< 4L) ~f:(fun i -> print_mem i (3L =:< 1L));
  print "\n";
  Range.Uns.iter (0L =:< 5L) ~f:(fun i -> print_mem i (1L =:= 3L));
  print "\n";
  Range.Uns.iter (0L =:< 3L) ~f:(fun i -> print_mem i (1L =:= 1L));
  print "\n";
  Range.Uns.iter (0L =:< 5L) ~f:(fun i -> print_mem i (3L =:= 1L))

let _ = test ()
