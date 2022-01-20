open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold Nat.([of_u64 0L; of_u64 1L;
      of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffn";
      of_string "0x8000_0000_0000_0000_0000_0000_0000_0000n"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "bits_to_zint/like_to_zint_opt "
        |> Nat.fmt ~alt:true ~radix:Radix.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> Zint.fmt ~alt:true ~radix:Radix.Hex ~pretty:true (Nat.bits_to_zint u)
        |> Fmt.fmt "/"
        |> (Option.fmt (Zint.fmt ~alt:true ~radix:Radix.Hex ~pretty:true)) (Nat.like_to_zint_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold Zint.([neg (of_string "0xffff_ffff_ffff_ffff_ffffz"); of_i64 (-1L); of_i64 0L;
      of_i64 1L; of_string "0xffff_ffff_ffff_ffff_ffffz"]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "bits_of_zint/like_of_zint_opt "
        |> Zint.fmt ~alt:true ~radix:Radix.Hex ~pretty:true i
        |> Fmt.fmt " -> "
        |> Nat.fmt ~alt:true ~radix:Radix.Hex ~pretty:true (Nat.bits_of_zint i)
        |> Fmt.fmt "/"
        |> (Option.fmt (Nat.fmt ~alt:true ~radix:Radix.Hex)) (Nat.like_of_zint_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
