open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "widen_to_nat_opt "
        |> I16.pp u
        |> Fmt.fmt " -> "
        |> (Option.fmt Nat.pp) (I16.widen_to_nat_opt u)
        |> Fmt.fmt "\n"
      );
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold Nat.([of_u64 0L; of_u64 1L; of_u64 32767L; of_u64 32768L; of_u64 65535L;
      of_u64 65536L; of_u64 131071L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "trunc_of_nat/narrow_of_nat_opt "
      |> Nat.pp u
      |> Fmt.fmt " -> "
      |> I16.fmt ~alt:true ~zpad:true ~width:4L ~radix:Radix.Hex ~pretty:true (I16.trunc_of_nat u)
      |> Fmt.fmt "/"
      |> (Option.fmt I16.pp) (I16.narrow_of_nat_opt u)
      |> Fmt.fmt "\n"
    )
  )
  |> ignore

let _ = test ()
