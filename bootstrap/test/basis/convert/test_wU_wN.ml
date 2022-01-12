open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U128.([of_u64 0L; of_u64 1L;
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "extend_to_nat "
        |> U128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> Nat.fmt ~alt:true ~base:Fmt.Hex ~pretty:true (U128.extend_to_nat u)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold Nat.([of_u64 0L; of_u64 1L;
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffn";
      of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000n"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "trunc_of_nat/narrow_of_nat_opt "
        |> Nat.fmt ~alt:true ~base:Fmt.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> U128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true (U128.trunc_of_nat u)
        |> Fmt.fmt "/"
        |> (Option.fmt U128.pp) (U128.narrow_of_nat_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
