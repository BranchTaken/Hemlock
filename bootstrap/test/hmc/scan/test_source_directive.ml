open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str {|[:"Foo.hm":42:13]|};
  scan_str {|[:"Foo.hm":42]|};
  scan_str {|[:"Foo.hm"]|};
  scan_str {|[:42:13]|};
  scan_str {|[:42]|};
  scan_str {|[:]|};

  scan_str {|[:10123456789]|};
  scan_str {|[:1] [:2]|};
  scan_str {|[:"Foo.hm"] [:] [:"Bar.hm"]|};
  scan_str {|[:"Foo.hm":1:42]a
b|};

  (* Errors. *)
  scan_str {|[:"Foo.hm":42:]|};
  scan_str {|[:"Foo.hm"::13]|};
  scan_str {|[::42:13]|};
  scan_str {|[:"Foo.hm"42:13]|};
  scan_str {|[:"Foo.hm:42:13]|};
  scan_str {|[:Foo.hm":42:13]|};
  scan_str {|[:":42:13]|};

  scan_str {|[:"Foo.hm":042:13]|};
  scan_str {|[:"Foo.hm":42:013]|};

  scan_str {|[:"Foo.hm":9999999999999999999]|};
  scan_str {|[:"Foo.hm":1:9999999999999999999]|};

  scan_str {|[:_"Foo.hm":42:13]|};
  scan_str {|[:"Foo.hm"_:42:13]|};
  scan_str {|[:"Foo.hm":_42:13]|};
  scan_str {|[:"Foo.hm":42_:13]|};
  scan_str {|[:"Foo.hm":42:_13]|};
  scan_str {|[:"Foo.hm":42:13_]|};

  scan_str {|[:"Foo.hm":42:13|};
  scan_str {|[:"Foo.hm":42:|};
  scan_str {|[:"Foo.hm":42|};
  scan_str {|[:"Foo.hm":|};
  scan_str {|[:"Foo.hm"|};
  scan_str {|[:"Foo.hm|};
  scan_str {|[:"|};

  scan_str {|[:"]|}

let _ = test ()
