open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str {|:1
|};
  scan_str {|:10123456789
|};
  scan_str {|:42 "foo.hm"
|};

  (* Errors. *)
  scan_str {|:a|};
  scan_str {|:0|};
  scan_str {|:1 |};
  scan_str {|:1"|};
  scan_str {|:1 "foo.hm|};
  scan_str {|:1 "foo.hm" |};
  scan_str {|:1 "foo.hm"x
|}

let _ = test ()
