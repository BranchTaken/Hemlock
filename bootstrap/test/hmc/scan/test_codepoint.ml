open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str "'a'";
  scan_str "'\\t' '\\n' '\\r' '\\'' '\\\\'";
  scan_str "'\\u{41}'";
  scan_str "'\\u{000_ff_fd}'";
  scan_str "'�'";

  (* Type parameter sigils. *)
  scan_str "'";
  scan_str "' ";
  scan_str "'\n";
  scan_str "'\n'";
  scan_str "' a";
  scan_str "'a";
  scan_str "'abcdefghijklmnopqrstuvwxyz_";
  scan_str "'aa'";
  scan_str "'(a: type)";
  scan_str "'\n  (a: type)";
  scan_str "x 'a: '(b: type) -> a -> b";

  (* Errors. *)
  scan_str "'\t' '\r'"; (* Raw tab/return. '\n' is treated as two tick tokens. *)
  scan_str "'\\\n";
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
  scan_str "''"

let _ = test ()
