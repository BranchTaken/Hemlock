open! Basis.Rudiments
open! Basis
open Format

let xpp_l xpp_a xppf = function
  | RangeIntf.Overflow -> Format.fprintf xppf "Overflow"
  | RangeIntf.Length a -> Format.fprintf xppf "Length %a" xpp_a a

let test () =
  printf "@[<h>";
  printf "length (0 .. 0) -> %Lu\n" (Range.length (0L =:< 0L));
  printf "length (0 .. 3) -> %Lu\n" (Range.length (0L =:< 3L));
  printf "length (253u8 .. 2u8) -> %Lu\n"
    (U8.extend_to_uns RangeH.U8.(length (U8.kv 253L =:< U8.kv 2L)));
  printf "length (-2i .. 2i) -> %Lu\n"
    (bits_of_sint RangeH.Sint.(length (Sint.kv (-2L) =:< Sint.kv 2L)));

  printf "length (0 ..= 0) -> %a\n" (xpp_l Uns.xpp) RangeF.Uns.(length (0L =:= 0L));
  printf "length (0 ..= 3) -> %a\n" (xpp_l Uns.xpp) RangeF.Uns.(length (0L =:= 3L));
  printf "length (2 ..= 1) -> %a\n" (xpp_l Uns.xpp) RangeF.Uns.(length (2L =:= 1L));

  printf "length (253u8 ..= 2u8) -> %a\n" (xpp_l U8.xpp) RangeF.U8.(length (U8.kv 253L =:= U8.kv 2L));
  printf "length (2u8 ..= 1u8) -> %a\n" (xpp_l U8.xpp) RangeF.U8.(length (U8.kv 2L =:= U8.kv 1L));
  printf "length (3u8 ..= 1u8) -> %a\n" (xpp_l U8.xpp) RangeF.U8.(length (U8.kv 3L =:= U8.kv 1L));
  printf "length (1u8 ..= 255u8) -> %a\n" (xpp_l U8.xpp) RangeF.U8.(length (U8.kv 1L =:= U8.kv 255L));
  printf "length (255u8 ..= 0u8) -> %a\n" (xpp_l U8.xpp) RangeF.U8.(length (U8.kv 255L =:= U8.kv 0L));
  printf "length (0u8 ..= 255u8) -> %a\n" (xpp_l U8.xpp) RangeF.U8.(length (U8.kv 0L =:= U8.kv 255L));
  printf "length (1u8 ..= 0u8) -> %a\n" (xpp_l U8.xpp) RangeF.U8.(length (U8.kv 1L =:= U8.kv 0L));
  printf "@]"

let _ = test ()
