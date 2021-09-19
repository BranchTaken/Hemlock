open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest
open Format

let test () =
  printf "@[<h>";
  scan_str {|""|};
  scan_str {|"
"|};
  scan_str {|"a \n \t \n \r \" \\ \u{41} \u{000_ff_fd}"|};

  scan_str {|"\u{110ffff}"|};
  scan_str {|"\u{110000}"|};
  scan_str {|"\u{110000}"|};
  scan_str {|"\u{d800}"|}; (* Surrogate. *)
  scan_str {|"\u{x"|};
  scan_str {|"\u{0"|};
  scan_str {|"\u{"|};
  scan_str {|"\u0"|};
  scan_str {|"\u"|};
  scan_str {|"\x"|};
  scan_str {|"""|};
  scan_str {|"|};
  scan_str {|"\u\v\w"|};
  printf "@]"

let _ = test ()
