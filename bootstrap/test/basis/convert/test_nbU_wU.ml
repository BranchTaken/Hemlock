open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "extend_to_u512 "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> U512.pp (U8.extend_to_u512 u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold U512.([of_u64 0L; of_u64 1L; of_u64 255L; of_u64 256L; of_u64 511L]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "trunc_of_u512/narrow_of_u512_opt "
        |> U512.pp u
        |> Fmt.fmt " -> "
        |> U8.pp (U8.trunc_of_u512 u)
        |> Fmt.fmt "/"
        |> (Option.fmt U8.pp) (U8.narrow_of_u512_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
