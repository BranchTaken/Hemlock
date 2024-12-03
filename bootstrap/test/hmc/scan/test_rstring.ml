open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
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

  scan_str {|``...``|};
  scan_str {|``...`...``|};
  scan_str {|`_`...``...`_`|};
  scan_str {|`_``...`_`|};
  scan_str {|`_`...``_`|};
  scan_str {|`__`...``...`_`...`__`|};
  scan_str {|`__`_`...``...`__`|};
  scan_str {|`__`...``...`_`__`|};
  scan_str {|`_x_`...``...`_`...`__`...`_x_`|};
  scan_str {|`_x_`__`...``...`_`...`_x_`|};
  scan_str {|`_x_`...``...`_`...`__`_x_`|};
  scan_str
    {|`_xx_`...``...`_`...`__`...`__`_0_`_1_`_2_`_3_`_4_`_5_`_6_`_7_`_8_`_9_`_a_`_b_`_c_`_d_`_e_`_f_...`_xx_`|};

  scan_str {|``�``|};
  scan_str {|`__`...`�_`_�...`__`|};

  (* Errors. *)
  scan_str {|` `aoeu` `|};
  scan_str {|`*`aoeu`*`|};

  scan_str{|`|};
  scan_str{|``|};
  scan_str{|```|};
  scan_str{|`tag``tag|};
  scan_str{|`tag``ta|};
  scan_str{|`tag``|};
  scan_str{|`tag`|};
  scan_str{|`tag|}

let _ = test ()
