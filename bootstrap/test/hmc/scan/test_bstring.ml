open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest
open Format

let test () =
  printf "@[<h>";
  scan_str {|
`|
`|};

  scan_str {|
`|a
`|};

  scan_str {|
`|a
 |b
`|};

  scan_str {|
`|
 |a
 |b
 |
`|};

  scan_str {|`||};
  scan_str {|
`|
 |};
  scan_str {|
`|
 ||};

  scan_str {|
`|a
|b
 |c
  |d
`|};
  printf "@]"

let _ = test ()
