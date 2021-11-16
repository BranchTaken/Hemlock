open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "extend_to_i512 "
        |> I16.pp i
        |> Fmt.fmt " -> "
        |> I512.pp (I16.extend_to_i512 i)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I512.([of_i64 (-32769L); of_i64 (-32768L); of_i64 (-1L); of_i64 0L; of_i64 1L;
      of_i64 32767L; of_i64 32768L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_i512/narrow_of_i512_opt "
      |> I512.pp u
      |> Fmt.fmt " -> "
      |> I16.pp (I16.trunc_of_i512 u)
      |> Fmt.fmt "/"
      |> (Option.fmt I16.pp) (I16.narrow_of_i512_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> ignore

let _ = test ()
