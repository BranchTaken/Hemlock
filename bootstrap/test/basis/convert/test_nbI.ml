open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "extend_to_uns "
        |> I16.pp i
        |> Fmt.fmt " -> "
        |> Uns.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex (I16.extend_to_uns i)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold [0L; 1L; 32767L; 32768L; 65535L] ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_uns/narrow_of_uns_opt "
      |> Uns.pp u
      |> Fmt.fmt " -> "
      |> I16.pp (I16.trunc_of_uns u)
      |> Fmt.fmt "/"
      |> (Option.fmt I16.pp) (I16.narrow_of_uns_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "extend_to_sint "
        |> I16.pp u
        |> Fmt.fmt " -> "
        |> Sint.pp (I16.extend_to_sint u)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold Sint.([kv (-32769L); kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L; kv 32768L;
      kv 65535L]) ~init:formatter ~f:(fun formatter i ->
      formatter
      |> Fmt.fmt "trunc_of_sint/narrow_of_sint_opt "
      |> Sint.pp i
      |> Fmt.fmt " -> "
      |> I16.pp (I16.trunc_of_sint i)
      |> Fmt.fmt "/"
      |> (Option.fmt I16.pp) (I16.narrow_of_sint_opt i)
      |> Fmt.fmt "\n"
    )
  )
  |> ignore

let _ = test ()
