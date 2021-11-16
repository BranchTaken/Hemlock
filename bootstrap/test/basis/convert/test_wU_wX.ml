open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold U128.([of_u64 0L; of_u64 1L; of_u64 127L; of_u64 128L; of_u64 255L]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "extend_to_i512 "
        |> U128.pp u
        |> Fmt.fmt " -> "
        |> I512.pp (U128.extend_to_i512 u)
        |> Fmt.fmt "\n"
      )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I512.([of_i64 (-1L); of_i64 0L; of_i64 1L;
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512";
      of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000i512"]) ~init:formatter
      ~f:(fun formatter u ->
        formatter
        |> Fmt.fmt "trunc_of_i512/narrow_of_i512_opt "
        |> I512.fmt ~alt:true ~base:Fmt.Hex ~pretty:true u
        |> Fmt.fmt " -> "
        |> U128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true (U128.trunc_of_i512 u)
        |> Fmt.fmt "/"
        |> (Option.fmt U128.pp) (U128.narrow_of_i512_opt u)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
