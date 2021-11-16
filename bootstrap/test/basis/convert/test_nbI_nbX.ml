open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "extend_to_i32 "
        |> I16.pp u
        |> Fmt.fmt " -> "
        |> I32.pp (I16.extend_to_i32 u)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I32.([kv (-32769L); kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L; kv 32768L])
      ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_i32/narrow_of_i32_opt "
      |> I32.pp u
      |> Fmt.fmt " -> "
      |> I16.pp (I16.trunc_of_i32 u)
      |> Fmt.fmt "/"
      |> (Option.fmt I16.pp) (I16.narrow_of_i32_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> ignore

let _ = test ()
