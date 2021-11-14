open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str {|""|};
  scan_str {|````|};
  scan_str {|``
``|};
  scan_str {|``

``|};
  scan_str {|``


``|};
  scan_str {|``a
b``|};
  scan_str {|``
a
b
``|};
  scan_str {|``a``|};
  scan_str {|`aoeu_``htns`gcrl`htns``aoeu_`|};
  scan_str {|``a\u{0}\t\n\r\"\\\
b``|};

  scan_str{|`|};
  scan_str{|``|};
  scan_str{|```|};
  scan_str{|`tag``tag|};
  scan_str{|`tag``ta|};
  scan_str{|`tag``|};
  scan_str{|`tag`|};
  scan_str{|`tag|}

let _ = test ()
