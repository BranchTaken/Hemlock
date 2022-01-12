open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  (* Zero. *)
  scan_str "0.";
  scan_str "0.0";
  scan_str "00.0";
  scan_str "00.00";

  scan_str "0e0";
  scan_str "0e-0";
  scan_str "0e+0";
  scan_str "0e00";

  (* Suffixes. *)
  scan_str "0r";
  scan_str "0r64";
  scan_str "0r32";

  (* Base-specific digits. *)
  scan_str "0b10_._01";
  scan_str "0o76543210_._01234567";
  scan_str "9876753210_._0123456789";
  scan_str "0xfedcba98.0 0x0.89abcdef";
  scan_str "0x76543210_.0 0x0._01234567";

  (* Min/max constants. *)
  scan_str "0b1p-1074";
  scan_str "0o1p-1074";
  scan_str "0x1p-1074";
  scan_str "0x0.0000_0000_0000_1p-1022";
  scan_str "0x1.ffff_ffff_ffff_fp1023";

  scan_str "0b1p-149r32";
  scan_str "0o1p-149r32";
  scan_str "0x1p-149r32";
  scan_str "0x0.0000_02p-126r32";
  scan_str "0x1.ffff_fep127r32";

  (* Miscellaneous. *)
  scan_str "1.0";
  scan_str "1_000_000.0";
  scan_str "42.e44";
  scan_str "42.3e-78";
  scan_str "1.5r32";
  scan_str "1.234_567_e_+89_r32";

  scan_str "0x0r";
  scan_str "0x1r";
  scan_str "0x3r";
  scan_str "0xffr";
  scan_str "0x1p42";
  scan_str "0x0.1p42";
  scan_str "0x0.01p42";
  scan_str "0x0.001p42";
  scan_str "0x0.0001p42";

  scan_str "0x0.1";
  scan_str "0x0.01";
  scan_str "0x00.001";
  scan_str "0x00.0001";

  scan_str "0b1.101p42";
  scan_str "0o7.406p42";
  scan_str "0x4.a3";
  scan_str "0x4a.3d2p+42";
  scan_str "0x4a.3d2p+42_r32";
  scan_str "0x4a.3d2p0";
  scan_str "0x4a.3d2p-42";

  scan_str "0x0p0";
  scan_str "0x0p-0";
  scan_str "0x0p1";

  (* Invalid. *)
  scan_str "0x1.z";
  scan_str "0x1px5";
  scan_str "0x1p0x5";
  scan_str "0x1p0x5y";
  scan_str "0r42";
  scan_str "0r032";
  scan_str "0r3x2";
  scan_str "0x1.zpxyr042";
  scan_str "0x3.f_ffff_e_r32";

  scan_str "0x3.f_ffff_ffff_ffff";
  scan_str "0x1p-1075";
  scan_str "0x0.0000_0000_0000_1p-1023";
  scan_str "0x1p1024";
  scan_str "0x1.ffff_ffff_ffff_f8p1023";

  scan_str "0x3.ffff_fer32";
  scan_str "0x1p-150r32";
  scan_str "0x0.0000_01p-126r32";
  scan_str "0x1.ffff_ffp127r32";

  scan_str "0b10_._012";
  scan_str "0o76543210_._012345678";
  scan_str "9876753210_._0123456789a";
  scan_str "0xgfedcba98.0 0x0.89abcdefg"

let _ = test ()
