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

  scan_str "0b01_01";
  scan_str "0o0123_4567";
  scan_str "0x0123_4567_89ab_cdef";

  scan_str "0u";
  scan_str "0i";
  scan_str "0u8";
  scan_str "0i8";

  scan_str "42u";
  scan_str "42i";
  scan_str "42u8";
  scan_str "42i8";

  (* Errors. *)
  scan_str "0a";
  scan_str "0AB42CD77";
  scan_str "0AB42CD77i";
  scan_str "0b2";
  scan_str "0o8";
  scan_str "0xg";

  scan_str "0u7";

  scan_str "0xffu8 0x100u8";
  scan_str "0x80i8 0x81i8";

  (* Miscellaneous. *)
  scan_str "0";
  scan_str "1";
  scan_str "-1";
  scan_str "42";

  scan_str "0b1010_1011u8";
  scan_str "0o253u8";
  scan_str "0xabu8";

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
