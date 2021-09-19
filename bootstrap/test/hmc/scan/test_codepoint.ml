open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest
open Format

let test () =
  printf "@[<h>";
  scan_str "'a' '\n'";
  scan_str "'\\t' '\\n' '\\r' '\\'' '\\\\'";
  scan_str "'\\u{41}'";
  scan_str "'\\u{000_ff_fd}'";

  (* Type parameter sigils. *)
  scan_str "'";
  scan_str "' ";
  scan_str "'\n";
  scan_str "'\\\n";
  scan_str "' a";
  scan_str "'a";
  scan_str "'abcdefghijklmnopqrstuvwxyz_";
  scan_str "'aa'";

  scan_str "'\\u{0}x'";
  scan_str "'\\u{110ffff}'";
  scan_str "'\\u{110000}'";
  scan_str "'\\u{110000}'";
  scan_str "'\\u{d800}'"; (* Surrogate. *)
  scan_str "'\\u{0z1}'";
  scan_str "'\\u{x'";
  scan_str "'\\u{0}a'";
  scan_str "'\\u{0}";
  scan_str "'\\u{0'";
  scan_str "'\\u{'";
  scan_str "'\\u00'";
  scan_str "'\\u0'";
  scan_str "'\\u'";
  scan_str "'\\u";
  scan_str "'\\x'";
  scan_str "'\\";
  scan_str "'''";
  scan_str "''";
  printf "@]"

let _ = test ()
