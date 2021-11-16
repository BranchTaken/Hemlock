open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "extend_to_i512 "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> I512.pp (U8.extend_to_i512 u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I512.([of_i64 (-1L); of_i64 0L; of_i64 1L; of_i64 255L; of_i64 256L]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "trunc_of_i512/narrow_of_i512_opt "
        |> I512.pp i
        |> Fmt.fmt " -> "
        |> U8.pp (U8.trunc_of_i512 i)
        |> Fmt.fmt "/"
        |> (Option.fmt U8.pp) (U8.narrow_of_i512_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
