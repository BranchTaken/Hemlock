open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "extend_to_uns "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> Uns.pp (U8.extend_to_uns u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold [0L; 1L; 255L; 256L; 511L] ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_uns/narrow_of_uns_opt "
      |> Uns.pp u
      |> Fmt.fmt " -> "
      |> U8.pp (U8.trunc_of_uns u)
      |> Fmt.fmt "/"
      |> (Option.fmt U8.pp) (U8.narrow_of_uns_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "extend_to_sint "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> Sint.pp (U8.extend_to_sint u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold Sint.([kv (-1L); kv 0L; kv 1L; kv 255L; kv 256L; kv 511L]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "trunc_of_sint/narrow_of_sint_opt "
        |> Sint.pp i
        |> Fmt.fmt " -> "
        |> U8.pp (U8.trunc_of_sint i)
        |> Fmt.fmt "/"
        |> (Option.fmt U8.pp) (U8.narrow_of_sint_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
