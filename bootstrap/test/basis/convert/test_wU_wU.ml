open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U128.([of_u64 0L; of_u64 1L;
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "extend_to_u512 "
        |> U128.fmt ~alt:true ~radix:Radix.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> U512.fmt ~alt:true ~radix:Radix.Hex ~pretty:true (U128.extend_to_u512 u)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold U512.([of_u64 0L; of_u64 1L;
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512";
      of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000u512"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "trunc_of_u512/narrow_of_u512_opt "
        |> U512.fmt ~alt:true ~radix:Radix.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> U128.fmt ~alt:true ~radix:Radix.Hex ~pretty:true (U128.trunc_of_u512 u)
        |> Fmt.fmt "/"
        |> (Option.fmt U128.pp) (U128.narrow_of_u512_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
