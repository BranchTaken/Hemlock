open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  (* Valid. *)
  scan_str "0";
  scan_str "00_";
  scan_str "01234567890_";
  scan_str "11";
  scan_str "22";
  scan_str "33";
  scan_str "44";
  scan_str "55";
  scan_str "66";
  scan_str "77";
  scan_str "88";
  scan_str "99";

  scan_str "0b_01_01";
  scan_str "0o_0123_4567";
  scan_str "0x_0123_4567_89ab_cdef";

  scan_str "0b10u8 0b10u16 0b10u32 0b10 0b10u 0b10u64 0b10u128 0b10u256 0b10u512 0b10n";
  scan_str "0b10i8 0b10i16 0b10i32 0b10i 0b10i64 0b10i128 0b10i256 0b10i512 0b10z";

  scan_str "0o76u8 0o76u16 0o76u32 0o76 0o76u 0o76u64 0o76u128 0o76u256 0o76u512 0o76n";
  scan_str "0o76i8 0o76i16 0o76i32 0o76i 0o76i64 0o76i128 0o76i256 0o76i512 0o76z";

  scan_str "98u8 98u16 98u32 98 98u 98u64 98u128 98u256 98u512 98n";
  scan_str "98i8 98i16 98i32 98i 98i64 98i128 98i256 98i512 98z";

  scan_str "0xfeu8 0xfeu16 0xfeu32 0xfe 0xfeu 0xfeu64 0xfeu128 0xfeu256 0xfeu512 0xfen";
  scan_str "0xfei8 0xfei16 0xfei32 0xfei 0xfei64 0xfei128 0xfei256 0xfei512 0xfez";

  scan_str "0L";
  scan_str "0u";
  scan_str "0i";
  scan_str "0u8";
  scan_str "0i8";
  scan_str "0n";
  scan_str "0z";

  scan_str "42L";
  scan_str "42u";
  scan_str "42i";
  scan_str "42u8";
  scan_str "42i8";

  scan_str "0xffu8";
  scan_str "0x80i8";

  (* Errors. *)
  scan_str "0a";
  scan_str "0AB42CD77";
  scan_str "0AB42CD77i";
  scan_str "0b2";
  scan_str "0o8";
  scan_str "0xg";

  scan_str "0L1";
  scan_str "0u7";
  scan_str "0n1";
  scan_str "0z1";

  scan_str "0x100u8";
  scan_str "0x81i8";

  scan_str "0b";
  scan_str "0o";
  scan_str "0x";

  (* Miscellaneous. *)
  scan_str "0";
  scan_str "1";
  scan_str "-1";
  scan_str "42";

  scan_str "0b1010_1011u8";
  scan_str "0o253u8";
  scan_str "0xabu8";

  scan_str "0x15L";
  scan_str "15L";
  scan_str "15u";
  scan_str "17u64";
  scan_str "0x0123_4567_89ab_cdef";
  scan_str "0o660";
  scan_str "0b10_0001";
  scan_str "0b0100_0001";
  scan_str "1_000_000";
  scan_str "0x___1_fffd";
  scan_str "17i64";
  scan_str "0x_ab__c_i";
  scan_str "0o777"

let _ = test ()
