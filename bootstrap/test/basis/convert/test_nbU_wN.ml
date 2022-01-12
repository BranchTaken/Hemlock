open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~init:formatter ~f:(fun formatter u ->
      formatter
      |> Fmt.fmt "extend_to_nat "
      |> U8.pp u
      |> Fmt.fmt " -> "
      |> Nat.pp (U8.extend_to_nat u)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold Nat.([of_u64 0L; of_u64 1L; of_u64 255L; of_u64 256L; of_u64 511L]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "trunc_of_nat/narrow_of_nat_opt "
        |> Nat.pp u
        |> Fmt.fmt " -> "
        |> U8.pp (U8.trunc_of_nat u)
        |> Fmt.fmt "/"
        |> (Option.fmt U8.pp) (U8.narrow_of_nat_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
