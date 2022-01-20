open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "widen_to_u512_opt "
        |> I16.pp u
        |> Fmt.fmt " -> "
        |> (Option.fmt U512.pp) (I16.widen_to_u512_opt u)
        |> Fmt.fmt "\n"
      );
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold U512.([of_u64 0L; of_u64 1L; of_u64 32767L; of_u64 32768L; of_u64 65535L;
      of_u64 65536L; of_u64 131071L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_u512/narrow_of_u512_opt "
      |> U512.pp u
      |> Fmt.fmt " -> "
      |> I16.fmt ~alt:true ~zpad:true ~width:4L ~radix:Radix.Hex ~pretty:true (I16.trunc_of_u512 u)
      |> Fmt.fmt "/"
      |> (Option.fmt I16.pp) (I16.narrow_of_u512_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> ignore

let _ = test ()
