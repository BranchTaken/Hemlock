open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U128.([of_u64 0L; of_u64 1L;
      of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128";
      of_string "0x8000_0000_0000_0000_0000_0000_0000_0000u128"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "bits_to_i128/like_to_i128_opt "
        |> U128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> I128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true (U128.bits_to_i128 u)
        |> Fmt.fmt "/"
        |> (Option.fmt (I128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true)) (U128.like_to_i128_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I128.([min_value; of_i64 (-1L); of_i64 0L; of_i64 1L; max_value]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "bits_of_i128/like_of_i128_opt "
        |> I128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true i
        |> Fmt.fmt " -> "
        |> U128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true (U128.bits_of_i128 i)
        |> Fmt.fmt "/"
        |> (Option.fmt U128.pp) (U128.like_of_i128_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
