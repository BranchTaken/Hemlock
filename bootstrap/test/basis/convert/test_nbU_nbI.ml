open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "bits_to_i8/like_to_i8_opt "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> I8.pp (U8.bits_to_i8 u)
      |> Fmt.fmt "/"
      |> (Option.fmt I8.pp) (U8.like_to_i8_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I8.([kv (-128L); kv (-1L); kv 0L; kv 1L; kv 127L]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "bits_of_i8/like_of_i8_opt "
        |> I8.pp i
        |> Fmt.fmt " -> "
        |> U8.pp (U8.bits_of_i8 i)
        |> Fmt.fmt "/"
        |> (Option.fmt U8.pp) (U8.like_of_i8_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
