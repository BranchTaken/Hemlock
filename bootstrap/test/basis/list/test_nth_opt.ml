open! Basis.Rudiments
open! Basis
open List

let test () =
  let l = [0L; 1L] in
  Range.iter (0L =:< 3L) ~f:(fun i ->
    match nth_opt i l with
    | None -> File.Fmt.stdout |> Uns.pp i |> Fmt.fmt " -> None\n" |> ignore
    | Some x ->
      File.Fmt.stdout |> Uns.pp i |> Fmt.fmt " -> Some " |> Uns.pp x |> Fmt.fmt "\n" |> ignore
  )

let _ = test ()
