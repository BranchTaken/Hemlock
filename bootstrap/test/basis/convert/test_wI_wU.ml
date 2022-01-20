open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I128.([of_string "0x8000_0000_0000_0000_0000_0000_0000_0000i128";
      of_i64 (-1L); of_i64 0L; of_i64 1L;
      of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi128"]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "widen_to_u512_opt "
        |> I128.fmt ~alt:true ~radix:Radix.Hex ~pretty:true i
        |> Fmt.fmt " -> "
        |> (Option.fmt (U512.fmt ~alt:true ~radix:Radix.Hex ~pretty:true)) (I128.widen_to_u512_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold U512.([of_u64 0L; of_u64 1L;
      of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512";
      of_string "0x8000_0000_0000_0000_0000_0000_0000_0000u512";
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512";
      of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000u512";
      of_string "0x1_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "trunc_of_u512/narrow_of_u512_opt "
        |> U512.fmt ~alt:true ~radix:Radix.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> I128.fmt ~alt:true ~radix:Radix.Hex ~pretty:true (I128.trunc_of_u512 u)
        |> Fmt.fmt "/"
        |> (Option.fmt (I128.fmt ~alt:true ~radix:Radix.Hex ~pretty:true)) (I128.narrow_of_u512_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
