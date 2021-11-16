open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "extend_to_i32 "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> I32.pp (U8.extend_to_i32 u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I32.([kv (-1L); kv 0L; kv 1L; kv 255L; kv 256L]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "trunc_of_i32/narrow_of_i32_opt "
        |> I32.pp i
        |> Fmt.fmt " -> "
        |> U8.pp (U8.trunc_of_i32 i)
        |> Fmt.fmt "/"
        |> (Option.fmt U8.pp) (U8.narrow_of_i32_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
