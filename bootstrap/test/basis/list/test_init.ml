open! Basis.Rudiments
open! Basis
open List

let test () =
  Range.iter (0L =:< 4L) ~f:(fun i ->
    Range.iter (0L =:< i) ~f:(fun j ->
      File.Fmt.stdout
      |> Fmt.fmt "("
      |> Uns.pp j
      |> Fmt.fmt " .. "
      |> Uns.pp i
      |> Fmt.fmt ") -> "
      |> (pp Uns.pp) (init (j =:< i) ~f:(fun k -> k))
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
