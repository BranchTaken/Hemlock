open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest
open Format

let test () =
  printf "@[<h>";

  scan_str "0.";
  scan_str "0.0";
  scan_str "00.0";
  scan_str "00.00";

  scan_str "0e0";
  scan_str "0e-0";
  scan_str "0e+0";
  scan_str "0e00";

  scan_str "0r";
  scan_str "0r64";
  scan_str "0r32";
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

  scan_str "0x3.f_ffff_ffff_ffff";
  scan_str "0x1p-1023";
  scan_str "0x1p1024";

  scan_str "0x3.f_ffff_e_r32";
  scan_str "0x1p-127_r32";
  scan_str "0x1p128_r32";

  printf "@]"

let _ = test ()
