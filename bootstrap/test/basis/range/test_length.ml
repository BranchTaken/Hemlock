open! Basis.Rudiments
open! Basis

let print s =
  File.Fmt.stdout |> Fmt.fmt s |> ignore

let test () =
  File.Fmt.stdout
  |> Fmt.fmt "length (0 .. 0) -> " |> Uns.pp (Range.length (0L =:< 0L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (0 .. 3) -> " |> Uns.pp (Range.length (0L =:< 3L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (253u8 .. 2u8) -> "
  |> Uns.pp (U8.extend_to_uns RangeH.U8.(length (U8.kv 253L =:< U8.kv 2L))) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (-2i .. 2i) -> "
  |> Uns.pp (bits_of_sint RangeH.Sint.(length (Sint.kv (-2L) =:< Sint.kv 2L))) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (0 ..= 0) -> " |> (Range.pp_l Uns.pp) RangeF.Uns.(length (0L =:= 0L))
  |> Fmt.fmt "\n"
  |> Fmt.fmt "length (0 ..= 3) -> " |> (Range.pp_l Uns.pp) RangeF.Uns.(length (0L =:= 3L))
  |> Fmt.fmt "\n"
  |> Fmt.fmt "length (2 ..= 1) -> " |> (Range.pp_l Uns.pp) RangeF.Uns.(length (2L =:= 1L))
  |> Fmt.fmt "\n"
  |> Fmt.fmt "length (253u8 ..= 2u8) -> "
  |> (Range.pp_l U8.pp) RangeF.U8.(length (U8.kv 253L =:= U8.kv 2L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (2u8 ..= 1u8) -> "
  |> (Range.pp_l U8.pp) RangeF.U8.(length (U8.kv 2L =:= U8.kv 1L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (3u8 ..= 1u8) -> "
  |> (Range.pp_l U8.pp) RangeF.U8.(length (U8.kv 3L =:= U8.kv 1L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (1u8 ..= 255u8) -> "
  |> (Range.pp_l U8.pp) RangeF.U8.(length (U8.kv 1L =:= U8.kv 255L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (255u8 ..= 0u8) -> "
  |> (Range.pp_l U8.pp) RangeF.U8.(length (U8.kv 255L =:= U8.kv 0L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (0u8 ..= 255u8) -> "
  |> (Range.pp_l U8.pp) RangeF.U8.(length (U8.kv 0L =:= U8.kv 255L)) |> Fmt.fmt "\n"
  |> Fmt.fmt "length (1u8 ..= 0u8) -> "
  |> (Range.pp_l U8.pp) RangeF.U8.(length (U8.kv 1L =:= U8.kv 0L)) |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
