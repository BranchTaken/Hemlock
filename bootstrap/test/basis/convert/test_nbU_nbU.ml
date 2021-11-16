open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "extend_to_u32 "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> U32.pp (U8.extend_to_u32 u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold U32.([kv 0L; kv 1L; kv 255L; kv 256L; kv 511L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_u32/narrow_of_u32_opt "
      |> U32.pp u
      |> Fmt.fmt " -> "
      |> U8.pp (U8.trunc_of_u32 u)
      |> Fmt.fmt "/"
      |> (Option.fmt U8.pp) (U8.narrow_of_u32_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> ignore

let _ = test ()
