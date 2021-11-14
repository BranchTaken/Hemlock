open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
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
`|}

let _ = test ()
