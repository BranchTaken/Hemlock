open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "widen_to_u32_opt "
        |> I16.pp i
        |> Fmt.fmt " -> "
        |> (Option.fmt U32.pp) (I16.widen_to_u32_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold U32.([kv (-32769L); kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L; kv 32768L;
      kv 65535L; kv 65536L; kv 131071L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_u32/narrow_of_u32_opt "
      |> U32.fmt ~alt:true ~zpad:true ~width:8L ~radix:Radix.Hex ~pretty:true u
      |> Fmt.fmt " -> "
      |> I16.fmt ~alt:true ~zpad:true ~width:4L ~radix:Radix.Hex ~pretty:true (I16.trunc_of_u32 u)
      |> Fmt.fmt "/"
      |> (Option.fmt I16.pp) (I16.narrow_of_u32_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> ignore

let _ = test ()
