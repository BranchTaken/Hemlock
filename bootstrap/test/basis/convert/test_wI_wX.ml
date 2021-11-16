open! Basis.Rudiments
open! Basis

let test () =
  File.Fmt.stdout
  |> (fun formatter ->
    List.fold I128.([of_string "0x8000_0000_0000_0000_0000_0000_0000_0000i128"; of_i64 (-1L);
      of_i64 0L; of_i64 1L; of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi128"])
      ~init:formatter ~f:(fun formatter i ->
      formatter
      |> Fmt.fmt "extend_to_i512 "
      |> I128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true i
      |> Fmt.fmt " -> "
      |> I512.fmt ~alt:true ~base:Fmt.Hex ~pretty:true (I128.extend_to_i512 i)
      |> Fmt.fmt "\n"
    )
  )
  |> Fmt.fmt "\n"
  |> (fun formatter ->
    List.fold I512.([of_i64 (-1L); of_i64 0L; of_i64 1L;
      of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512";
      of_string "0x8000_0000_0000_0000_0000_0000_0000_0000i512";
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512";
      of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000i512";
      of_string "0x1_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512"]) ~init:formatter
      ~f:(fun formatter i ->
        formatter
        |> Fmt.fmt "trunc_of_i512/narrow_of_i512_opt "
        |> I512.fmt ~alt:true ~base:Fmt.Hex ~pretty:true i
        |> Fmt.fmt " -> "
        |> I128.fmt ~alt:true ~base:Fmt.Hex ~pretty:true (I128.trunc_of_i512 i)
        |> Fmt.fmt "/"
        |> (Option.fmt I128.pp) (I128.narrow_of_i512_opt i)
        |> Fmt.fmt "\n"
      )
  )
  |> ignore

let _ = test ()
