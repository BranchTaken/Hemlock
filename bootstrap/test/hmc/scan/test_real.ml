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

  (* e mantissa forms. *)
  scan_str "0e0";
  scan_str "1e0";
  scan_str "0.e0";
  scan_str "1.e0";
  scan_str "0.0e0";
  scan_str "0.1e0";
  scan_str "1.0e0";
  scan_str "1.1e0";
  scan_str "12e0";
  scan_str "12.e0";
  scan_str "12.3e0";
  scan_str "12.34e0";

  (* p mantissa forms. *)
  scan_str "0b0p0";
  scan_str "0b1p0";
  scan_str "0b0.p0";
  scan_str "0b1.p0";
  scan_str "0b0.0p0";
  scan_str "0b0.1p0";
  scan_str "0b1.0p0";
  scan_str "0b1.1p0";
  scan_str "0b10p0";
  scan_str "0b10.p0";
  scan_str "0b10.0p0";
  scan_str "0b10.1p0";
  scan_str "0b10.01p0";

  scan_str "0o0p0";
  scan_str "0o1p0";
  scan_str "0o0.p0";
  scan_str "0o1.p0";
  scan_str "0o0.0p0";
  scan_str "0o0.1p0";
  scan_str "0o1.0p0";
  scan_str "0o1.1p0";
  scan_str "0o12p0";
  scan_str "0o12.p0";
  scan_str "0o12.0p0";
  scan_str "0o12.3p0";
  scan_str "0o12.34p0";

  scan_str "0x0p0";
  scan_str "0x1p0";
  scan_str "0x0.p0";
  scan_str "0x1.p0";
  scan_str "0x0.0p0";
  scan_str "0x0.1p0";
  scan_str "0x1.0p0";
  scan_str "0x1.1p0";
  scan_str "0x12p0";
  scan_str "0x12.p0";
  scan_str "0x12.0p0";
  scan_str "0x12.3p0";
  scan_str "0x12.34p0";

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

  (* Underscores. *)
  scan_str "0b__0__.__0__p__+__0__r";
  scan_str "0o__0__.__0__p__+__0__r";
  scan_str "0__.__0__e__+__0__r";
  scan_str "0x__0__.__0__p__+__0__r";

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

  (************************************************************************************************)
  (* Invalid. *)

  (* Missing components. *)
  scan_str "0b.";
  scan_str "0b0.p";
  scan_str "0b0.pr64";
  scan_str "0b0.p+";
  scan_str "0b0.p+r64";
  scan_str "0b0.p+0r6";

  scan_str "0o.";
  scan_str "0o0.p";
  scan_str "0o0.pr64";
  scan_str "0o0.p+";
  scan_str "0o0.p+r64";
  scan_str "0o0.p+0r6";

  scan_str "0.e";
  scan_str "0.er64";
  scan_str "0.e+";
  scan_str "0.e+r64";
  scan_str "0.e+0r6";

  scan_str "0x.";
  scan_str "0x0.p";
  scan_str "0x0.pr64";
  scan_str "0x0.p+";
  scan_str "0x0.p+r64";
  scan_str "0x0.p+0r6";

  (* Type suffix. *)
  scan_str "0r42";
  scan_str "0r032";
  scan_str "0r3x2";
  scan_str "0x1.zpxyr042";

  (* Decimal p mantissa forms. *)
  scan_str "0p0";
  scan_str "1p0";
  scan_str "0.p0";
  scan_str "1.p0";
  scan_str "0.0p0";
  scan_str "0.1p0";
  scan_str "1.0p0";
  scan_str "1.1p0";
  scan_str "12p0";
  scan_str "12.p0";
  scan_str "12.3p0";
  scan_str "12.34p0";

  (* Non-decimal e mantissa forms. *)
  scan_str "0b0e0";
  scan_str "0b1e0";
  scan_str "0b0.e0";
  scan_str "0b1.e0";
  scan_str "0b0.0e0";
  scan_str "0b0.1e0";
  scan_str "0b1.0e0";
  scan_str "0b1.1e0";
  scan_str "0b10e0";
  scan_str "0b10.e0";
  scan_str "0b10.0e0";
  scan_str "0b10.1e0";
  scan_str "0b10.01e0";

  scan_str "0o0e0";
  scan_str "0o1e0";
  scan_str "0o0.e0";
  scan_str "0o1.e0";
  scan_str "0o0.0e0";
  scan_str "0o0.1e0";
  scan_str "0o1.0e0";
  scan_str "0o1.1e0";
  scan_str "0o12e0";
  scan_str "0o12.e0";
  scan_str "0o12.0e0";
  scan_str "0o12.3e0";
  scan_str "0o12.34e0";
  (* 0x0e0 etc. are valid because `e` is a valid digit. *)

  (* Out of range. *)
  scan_str "0x3.f_ffff_ffff_ffff";
  scan_str "0x1p-1075";
  scan_str "0x0.0000_0000_0000_1p-1023";
  scan_str "0x1p1024";
  scan_str "0x1.ffff_ffff_ffff_f8p1023";

  scan_str "0x3.ffff_fer32";
  scan_str "0x1p-150r32";
  scan_str "0x0.0000_01p-126r32";
  scan_str "0x1.ffff_ffp127r32";

  (* Invalid digits for base. *)
  scan_str "0b10_._012";
  scan_str "0o76543210_._012345678";
  scan_str "9876753210_._0123456789a";
  scan_str "0xgfedcba98.0 0x0.89abcdefg";

  (* Underscores. *)
  scan_str "0_b.";
  scan_str "0b0r_";
  scan_str "0b0r6_4";
  scan_str "0b0r64_";

  scan_str "0_o.";
  scan_str "0o0r_";
  scan_str "0o0r6_4";
  scan_str "0o0r64_";

  scan_str "0r_";
  scan_str "0r6_4";
  scan_str "0r64_";

  scan_str "0_x.";
  scan_str "0x0r_";
  scan_str "0x0r6_4";
  scan_str "0x0r64_";

  (* Miscellaneous. *)
  scan_str "0x1.z";
  scan_str "0x1px5";
  scan_str "0x1p0x5";
  scan_str "0x1p0x5y";
  scan_str "0x3.f_ffff_e_r32"

let _ = test ()
